# set the directory
setwd("C:\\Users\\Nika\\OneDrive\\Documents\\Master's\\Dissertation\\Data Section")

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
library(tidyr)
library(readr)

# loading individual data:
# unit of observation in individuals is an individual visit (meaning for one person there are multiple observations)
individuals <- read_csv('Data Files for Git/repeat_individual_questionnaire.csv')

##insert vax if wanted from 'Extra'##

# loading household data: 
# more than one observation per household
households <- read_csv('Data Files for Git/Submissions.csv')

##insert total bought from 'Extra'##
##insert total_assets from 'Extra'##

# get visit date and number from households into individuals
data <- left_join(individuals,households,by=c('parent_key'='key'))

# load wealth index dataset
wealth <- read_csv('Data Files for Git/wealth_index.csv')

# Join the above dataset with the wealth_index.csv dataset
data <- left_join(data,wealth,by=c('household_id'))

# read in and join all of the minicensus data
#
minicensus_data <- read_csv('Data Files for Git/minicensus_data.csv')
  
# 
minicensus_household_final <- read_csv('Data Files for Git/minicensus_household_final.csv')

#
minicensus_wealth_index <- read_csv('Data Files for Git/minicensus_wealth_index.csv')

#merge the minicensus files by hh_id
minicensus <- left_join(minicensus_data,minicensus_household_final, by=c('hh_id'))

#merge the last minicensus file by hh_id
minicensus <- left_join(minicensus, minicensus_wealth_index, by=c('hh_id'))

# read in the csv that matches household_id to hh_id
encryption <- read_csv('Data Files for Git/household_id_encryptions.csv')

# change the column name of anonymized_household_id to hh_id
encryption$hh_id <- encryption$anonymized_household_id

# merge encryption with minicensus
minicensus <- left_join(minicensus, encryption, by=c('hh_id'))

# merge minicensus and data
data <- left_join(minicensus, data, by=c('household_id'))

## insert breastfeeding from 'Extra' if wanted##

# make age_in_days variable (this accounts for leap years) - 
# this assumes the age was originally in years
data$age_in_days <- round(data$age * 365.25)

# make age_in_months variable
data <- data %>% mutate(age_in_months = age_in_days/30.4)

#merge weight and child_weight and height and child_height
data$merged_weight <- ifelse(!is.na(data$weight), data$weight, data$child_weight)
data$merged_height <- ifelse(!is.na(data$height), data$height, data$child_height)

# keep only the earliest observations that have weight and height 
# (if someone has many keep earliest)
data <- data %>% 
  arrange(todays_date) %>%
  filter(!is.na(merged_weight), !is.na(merged_height)) %>%
  filter(!is.na(sex),
         age_in_months <= 60) %>%
  dplyr::distinct(extid, .keep_all = TRUE)

# remove the crazy heights and weights
#data <- data %>% filter(merged_height <= 220, merged_weight <= 80)
# Calculate the IQR for height
Q1 <- quantile(data$merged_height, 0.25)
Q3 <- quantile(data$merged_height, 0.75)
IQR <- Q3 - Q1

# Define a range for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers based on IQR method
outliers_iqr <- data[data$merged_height < lower_bound | data$merged_height > upper_bound, ]

# Create a scatter plot of height vs weight
ggplot(data, aes(x = merged_height, y = merged_weight)) +
  geom_point() +
  labs(title = "Filtered Scatter Plot of Height vs Weight",
       x = "Height",
       y = "Weight")

# Create the growth chart
ggplot(data, aes(x = age_in_months)) +
  geom_line(aes(y = merged_height, color = "Height"), size = 1.5) +
  geom_line(aes(y = merged_weight, color = "Weight"), size = 1.5) +
  labs(title = "Growth Chart", x = "Age (Months)", y = "Measurement") +
  scale_color_manual(values = c("Height" = "blue", "Weight" = "red")) +
  theme_minimal()

# change female to f and male to m in sex column 
# (for zlen calculation)
data$sex <- ifelse(data$sex == "Female", "f", "m")

# make a dob column which we will use to remove anomalous stuff
data <- data %>%
  mutate(dob=as.Date(dob))

# remove anyone more than five since that is who I am focusing on
data <- data %>%
  filter(age <= 5)

# multiple zscore calculators have been tried: whoanthro, childsds; they yield the same as the anthro package below. Therefore, the anthro package is used.

# run the WHO's anthro package and save their output in a variable (library(anthro))
who_output <- with(
  data,
  anthro_zscores(
    sex = sex,
    age = age_in_days,
    weight = merged_weight,
    lenhei = merged_height
  )
)

# make a zlen column in data that comes from who_output 
# zlen is length/height-for-age z-score
data$zlen <- who_output$zlen

# z scores using cdc guide
# cdc_output<- cdcanthro(data, age = age_in_months, wt = merged_weight, ht = merged_height, all = FALSE)
# add the variable to the dataset
#data$mod_haz <- cdc_output$mod_haz

# drop NAs from mod_haz and zlen
data <- data %>%
  drop_na(zlen)
# data <- data %>% drop_na(mod_haz)

#correlation test between who and cdc [it is almost 1 so decided to use WHO calculator]
#cor(data$zlen, data$mod_haz)

# bland altman plot to assess difference of measurements
#ggplot(data, aes(x = (zlen + mod_haz) / 2, y = zlen - mod_haz)) + geom_point() + geom_hline(yintercept = 0, linetype = "dashed", color = "red")

# make a new variable called strange showing all of the strange variables
data <- data %>% mutate(strange=zlen<=-6|zlen>=6)

# making data the dataframe without the strange in zlen
data <- data %>%
  filter(!strange)

# making the data frame and creating a column that tells us if a child is stunted or not
data <- data %>%
  mutate(stunted = zlen <= -2)

# remove NAs from wealth columns
data <- data %>% 
  drop_na(wealth_index_rank)


#making the cleaned dataset with only necessary variables
clean <- select(data, household_id, ward_name, cluster, stunted, zlen, merged_weight, merged_height, age, age_in_months, age_in_days, dob, sex, hh_member_num, hh_n_constructions, n_nets_in_hh, member_per_sleep, hh_n_cows, hh_n_pigs, animals, hh_main_building_type, building_type_apartment, building_type_conventional_house, building_type_flat, building_type_hut, building_type_precarious, building_type_traditional_mud_house, building_type_other, hh_main_wall_material, hh_wall_adobe_block, hh_wall_bamboo, hh_wall_tin, hh_wall_bark, hh_wall_tinned_wood, hh_wall_palm_tree, hh_wall_brick_block, hh_wall_Other, cook_main_water_source, water_source_piped_water_house, water_source_piped_water_compound, water_source_piped_water_neighbor, water_source_fountain, water_source_protected_well_in_backyard, water_source_protected_well_out_backyard, water_sourcel_unprotected_well_in_household, water_source_unprotected_well_out_household, water_source_hole_man_pump_inside_household, water_source_hole_protected_hand_pump_yard, water_source_surface, water_source_rainwater, water_source_mineral_bottled_water, water_source_water_tank_truck, water_source_other, water_time_under_10_min, water_time_between_10_30_min, water_time_between_30_60_min, water_time_more_than_hour, lighting_energy_electricity, lighting_energy_generator, lighting_energy_solar_panel, lighting_energy_gas, lighting_energy_oil, lighting_energy_candles, lighting_energy_batteries, lighting_energy_firewood, lighting_energy_other, cook_time_to_water, hh_main_energy_source_for_lighting, hh_possession_Radio, hh_possession_TV, hh_possession_Cell_phone, any_deaths_past_year, hh_head_age, hh_head_gender, wealth_index_score, wealth_index_std_score, wealth_index_rank)

#rename merged_weight and merged_height to weight and height
names(clean)[names(clean) == "merged_height"] <- "height"
names(clean)[names(clean) == "merged_weight"] <- "weight"

# Now we will add the distance variables

# Get latitude and longitude variables associated with each household_id to calculate the following variables:
# if(FALSE){
#   # Below section is how locations were generated, does not need to be run again
#   census_wide_households <- read_csv('Data Section/tmp/crf/census_wide_households.csv')
#   household_locations <- census_wide_households %>% dplyr::select(household_id = hh_id,
#                                                                   lng, lat)
#   write_csv(household_locations, 'Data Section/tmp/crf/household_locations.csv')
# }
locations <- read_csv('Data Files for Git/household_locations.csv')

# Read in household ID encryptions
encryptions <- read_csv('Data Files for Git/household_id_encryptions.csv')

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
load('Data Files for Git/hf.RData')
# Sanity plot
#plot(hf)

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
load('Data Files for Git/roads.RData')
roads_projected <- spTransform(roads, proj_crs)

# sanity plot
#plot(roads_projected)

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

#make th distance from meters to kilometers
clean$admin_km <- data$admin_dist / 1000
clean$health_km <- data$health_dist / 1000
clean$road_km <- data$road_dist / 1000

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
write.csv(clean, "C:\\Users\\Nika\\OneDrive\\Documents\\Master's\\Dissertation\\Data Section\\Data Files for Git\\Dataset.csv", row.names=FALSE)

