# set directory
setwd("C:\\Users\\Nika\\OneDrive\\Documents\\Master's\\Dissertation\\Data Section")

# load libraries
#load libraries
library(kableExtra)
library(dplyr)
library(knitr)
library(fastmap)

# load data
data <- read.csv("Dataset.csv")

# make a table of characteristics of toddlers
options(knitr.table.format = "html") # to allow for a slightly complex table that RMarkdown doesn't support

# create a table for ward name with count and frequency while making a new column of its type
ward_table <- data %>%
  group_by(ward_name) %>%
  summarize(n = n(), frequency = n() / nrow(data))

# make the ward names in alphabetical order
ward_table <- ward_table %>%
  arrange('ward')

# create a table for sex with count and frequency while making a new column of its type
sex_table <- data %>%
  group_by(sex) %>%
  summarize(n = n(), frequency = n() / nrow(data))

#change f and m to Female and Male
sex_table <- sex_table %>%
  mutate(sex = ifelse(sex =='f', 'Female', 'Male'))

# create a table for age category with count and frequency while making a new column of its type
age_table <- data %>%
  group_by(age_category) %>%
  summarize(n = n(), frequency = n() / nrow(data))

# Make age categories in the proper order
age_category_order <- c('0-5', '6-11', '12-23', '24-35', '36-47', '48-60')

age_table <- age_table %>%
  arrange(factor(age_category, levels = age_category_order))

# create one table from the above
merged_table <- bind_rows(sex_table, age_table, ward_table)

# make a column of the descriptions
merged_table$characteristics <- coalesce(merged_table$sex,
                                merged_table$ward_name,
                                merged_table$age_category)

# remove not needed columns
merged_table <- merged_table %>%
  dplyr::select(-sex, -age_category, - ward_name)

#reorganise the columns
desired_order <- c('characteristics', 'n', 'frequency')

merged_table <- merged_table %>%
  dplyr::select(desired_order)

#rename the columns
merged_table <- merged_table %>%
  rename(Characteristics = characteristics,
         N = n,
         `%` = frequency)

#make frequency only 2 decimal points
merged_table$`%` <- round(merged_table$`%`, 3)

#Print the resulting table using kbl
kbl(merged_table, caption = 'Distribution of Characteristics of Children Under Five in Mopeia') %>%
  kable_styling(bootstrap_options = c('striped', 'condensed', 
                                      font_size = 12)) %>%
  pack_rows('Sex', 1, 2) %>% # pack_rows puts the rows in groups
  pack_rows('Age Category (months)', 3, 8) %>% # in quotes is the name
  pack_rows('Ward', 9, 16) %>% #the numbers are the selected rows %>%
  kable_minimal() 

#from notes
kbl(merged_table, caption = 'Distribution of Characteristics of Children Under Five in Mopeia') %>%
  kable_styling(bootstrap_options = c('hover', 'condensed', font_size = 12)) %>%
  pack_rows('Sex', 1, 2) %>%
  pack_rows('Age Category (months)', 3, 8) %>%
  pack_rows('Ward', 9, 16) %>%
  add_header_above(c(' ' = 1, 'Frequency' = 2)) %>%
  kable_minimal() %>%
  footnote(general = 'Insert Source',
               general_title = 'Source: ',
               title_format = c('italic', 'underline'))

#need to add wealth rank, maybe number of people in household, distances?, and primarily focus on finishing the regression.
