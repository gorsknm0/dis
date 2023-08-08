# set the directory
setwd("C:\\Users\\Nika\\OneDrive\\Documents\\Master's\\Dissertation")

#load libraries
library(dplyr)
library(readr)
library(ggplot2)
library(anthro)
library(tidyverse)
library(aod)
library(rgdal)
library(ggthemes)
library(sp)

# unit of observation in individuals is an individual visit (meaning for one person there are multiple observations)
individuals <- read_csv('Data Section/tmp/crf/repeat_individual_questionnaire.csv')

# more than one observation per household
households <- read_csv('Data Section/tmp/crf/Submissions.csv')

# get visit date and number from households into individuals
data <- left_join(individuals,households,by=c('parent_key'='key'))

# wealth index dataset
wealth <- read_csv('Data Section/tmp/crf/wealth_index.csv')

# Join the above dataset with the wealth_index.csv dataset
data <- left_join(data,wealth,by=c('household_id'))

#merge weight and child_weight and height and child_height
data$merged_weight <- ifelse(!is.na(data$weight), data$weight, data$child_weight)
data$merged_height <- ifelse(!is.na(data$height), data$height, data$child_height)

# keep only the earliest observations that have weight and height (if someone has many keep earliest)
data <- data %>% 
  arrange(todays_date) %>%
  filter(!is.na(merged_weight), !is.na(merged_height)) %>%
  filter(!is.na(sex),
         dob >= as.Date('2007-01-01')) %>%
  dplyr::distinct(extid, .keep_all = TRUE)

# Function to identify outliers using IQR
identify_outliers_iqr <- function(column) {
  q1 <- quantile(column, 0.25)
  q3 <- quantile(column, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  outliers <- column < lower_bound | column > upper_bound
  return(outliers)
}

# Identify and remove outliers using IQR
outliers_height_iqr <- identify_outliers_iqr(data$merged_height)
outliers_weight_iqr <- identify_outliers_iqr(data$merged_weight)

data <- data[!outliers_height_iqr & !outliers_weight_iqr, ]

# change female to f and male to m in sex column
data$sex <- ifelse(data$sex == "Female", "f", "m")

# make a dob column which we will use to remove anomalous stuff
data <- data %>%
  mutate(dob=as.Date(dob))

# make age_in_days variable (this accounts for leap years) - this assumes the age was originally in years
data$age_in_days <- round(data$age * 365.25)

data <- data %>%
  filter(age <= 5)

# run the WHO's anthro package and save their output in a variable
# QUESTION: does the anthro_output have the same order as data???
anthro_output <- with(
  data,
  anthro_zscores(
    sex = sex,
    age = age_in_days,
    weight = merged_weight,
    lenhei = merged_height
  )
)

# make a zlen column in data that comes from anthro_output 
# zlen is length/height-for-age z-score
data$zlen <- anthro_output$zlen

# make a new variable called strange showing all of the strange variables
data <- data %>%
  mutate(strange=zlen<=-4|zlen>=4)

# making data the dataframe without the strange in zlen
data <- data %>%
  filter(!strange)

# making the data frame and creating a column that tells us if a child is stunted or not
data <- data %>%
  mutate(stunted = zlen <= 0)

#making the cleaned dataset with only necessary variables
clean <- select(data, c(person_string, extid, household_id, hamlet_code_from_hhid, hamlet_name, village_name, ward_name, district_name, cluster, stunted, zlen, merged_weight, merged_height, age, age_in_days, dob, sex, roster_size, wealth_index_score, wealth_index_std_score, wealth_index_rank))

#rename merged_weight and merged_height to weight and height
names(clean)[names(clean) == "merged_height"] <- "height"
names(clean)[names(clean) == "merged_weight"] <- "weight"

# remove NAs from wealth columns
clean <- clean %>% 
  drop_na(wealth_index_rank)

# Now we will add the distance variables

# Get latitude and longitude variables associated with each household_id to calculate the following variables:
# if(FALSE){
#   # Below section is how locations were generated, does not need to be run again
#   census_wide_households <- read_csv('Data Section/tmp/crf/census_wide_households.csv')
#   household_locations <- census_wide_households %>% dplyr::select(household_id = hh_id,
#                                                                   lng, lat)
#   write_csv(household_locations, 'Data Section/tmp/crf/household_locations.csv')
# }
locations <- read_csv('Data Section/tmp/crf/household_locations.csv')

# Read in household ID encryptions
encryptions <- read_csv('Data Section/tmp/crf/household_id_encryptions.csv')

# Decrypt the household IDs
locations <- locations %>%
  dplyr::rename(anonymized_household_id = household_id) %>%
  left_join(encryptions) %>%
  dplyr::select(-anonymized_household_id)

# The above is now a table with latitude and longitude and household ID which can be joined to the clean data
clean <- left_join(clean, locations)

# Now "clean" has lng and lat columns

# Read in all health facilities
# hf <- rgdal::readOGR('Data Section/tmp/crf/health_facilities/', 'health_facilities')
# save(hf, file = 'Data Section/tmp/crf/hf.RData')
library(sp)
load('Data Section/tmp/crf/hf.RData')
# Sanity plot
plot(hf)

# health_dist: distance from the household to the health facility (Centro de Saúde de Mopeia Sede) [-17.979471355490286, 35.712640789708786]
# Create an object just for mopeia sede health facility
# mshf <- hf[hf@data$name == 'Centro de Saude de Mopeia Sede',]

mshf <- hf

# Project to UTM coordinates so as to measure meters
proj_crs <- CRS("+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
clean_spatial <- clean %>% mutate(x = lng, y = lat)
coordinates(clean_spatial) <- ~x+y
proj4string(clean_spatial) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
clean_spatial_projected <- spTransform(clean_spatial, proj_crs)
mshf_projected <- spTransform(mshf, proj_crs)

# Calculate distance in meters
d <- rgeos::gDistance(clean_spatial_projected, mshf_projected, byid = TRUE)

# calculate minimum distance from house to hospital
min_dist <- apply(d, 2, min) 

# Pop the distances into the dataframe
clean$health_dist <- as.numeric(min_dist)

# Sanity check the result
ggplot(data = clean,
       aes(x = lng,
           y = lat,
           color = health_dist)) +
  geom_point() +
  scale_color_gradient2(mid = 'red', high = 'black')

# road_dist: distance from the household to the road
# roads <- rgdal::readOGR('Data Section/tmp/crf/mopeia_roads/', 'mopeia_roads')
# save(roads, file = 'Data Section/tmp/crf/roads.RData')
load('Data Section/tmp/crf/roads.RData')
roads_projected <- spTransform(roads, proj_crs)

# sanity plot
plot(roads_projected)

# Not sure which is "main" road so just calculating distance to all roads
# Calculate distance in meters
d <- rgeos::gDistance(clean_spatial_projected, roads_projected, byid = TRUE)
dx <- apply(d, 2, min)

# Pop the distances into the dataframe
clean$road_dist <- as.numeric(dx)

# Sanity plot
roads_gg <- fortify(roads, id = 'osm_id')
ggplot() +
  geom_path(data = roads_gg,
            aes(x = long, y = lat, group = group)) +
  geom_point(data = clean,
             aes(x = lng, y = lat, color = road_dist),
             size = 0.2) +
  scale_color_gradient2(mid = 'red', high = 'black')

# create variable admin_dist: distance from the household to the administrative post (government building/ Governo de Distrito de Mopeia) with the lat/lon points [-17.981304088784032, 35.710798061452984]
admin <- tibble(x = 35.710798061452984,
                y = -17.981304088784032)
coordinates(admin) <- ~x+y
proj4string(admin) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
admin_projected <- spTransform(admin, proj_crs)

# Calculate distance in meters
d <- rgeos::gDistance(clean_spatial_projected, admin_projected, byid = TRUE)

# Pop the distances into the dataframe
clean$admin_dist <- as.numeric(d)

# make age groups: Category 1: 0-5, Category 2: 6-11, Category 3: 12-17, Category 4: 18-24 months
clean <- clean %>% 
  mutate(age_in_months = age_in_days/30.4)

clean <- clean %>%
  mutate(age_category = case_when(
    age_in_months >= 0 & age_in_months < 6 ~ '0-5',
    age_in_months >= 6 & age_in_months < 12 ~ '6-11',
    age_in_months >= 12 & age_in_months < 24 ~ '12-23',
    age_in_months >= 24 & age_in_months < 36 ~ '24-35',
    age_in_months >= 36 & age_in_months < 48 ~ '36-47',
    age_in_months >= 48 & age_in_months <= 61 ~ '48-60',
    TRUE ~ NA_character_))

#make a stunted column that is numeric
clean$stunted_numeric <- as.numeric(clean$stunted)

# make this into a csv file and save to computer 
write.csv(clean, "C:\\Users\\Nika\\OneDrive\\Documents\\Master's\\Dissertation\\Data Section\\Dataset.csv", row.names=FALSE)
