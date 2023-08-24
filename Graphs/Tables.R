# set directory
setwd("C:\\Users\\Nika\\OneDrive\\Documents\\Master's\\Dissertation\\Data Section")

# load libraries
#load libraries
library(kableExtra)
library(dplyr)
library(knitr)
library(fastmap)
library(gridExtra)
library(cowplot)

# load data
data <- read.csv("Data Files for Git/Dataset.csv")

# make a table of characteristics of toddlers
options(knitr.table.format = "html") # to allow for a slightly complex table that RMarkdown doesn't support

# create a table for ward name with count and frequency while making a new column of its type
ward_table <- data %>%
  group_by(ward_name) %>%
  summarize(n = n(), 
            frequency = (n() / nrow(data)) * 100,
            z_Score = (sum(zlen) / n()),
            stunted_percentage = (sum(stunted_numeric) / n()) * 100)

# make the ward names in alphabetical order
ward_table <- ward_table %>%
  arrange('ward')

# create a table for sex with count and frequency while making a new column of its type
sex_table <- data %>%
  group_by(sex) %>%
  summarize(n = n(), 
            frequency = (n() / nrow(data)) * 100,
            z_Score = (sum(zlen) / n()),
            stunted_percentage = (sum(stunted_numeric) / n()) * 100)

#change f and m to Female and Male
sex_table <- sex_table %>%
  mutate(sex = ifelse(sex =='f', 'Female', 'Male'))

# create a table for age category with count and frequency while making a new column of its type
age_table <- data %>%
  group_by(age_category) %>%
  summarize(n = n(), 
            frequency = (n() / nrow(data)) * 100,
            z_Score = (sum(zlen) / n()),
            stunted_percentage = (sum(stunted_numeric) / n()) * 100)

# Make age categories in the proper order
age_category_order <- c('0-5', '6-11', '12-23', '24-35', '36-47', '48-60')

age_table <- age_table %>%
  arrange(factor(age_category, levels = age_category_order))

# make wealth index table
wealth_table <- data %>%
  group_by(wealth_index_rank) %>%
  summarize(n = n(), 
            frequency = (n() / nrow(data)) * 100,
            z_Score = (sum(zlen) / n()),
            stunted_percentage = (sum(stunted_numeric) / n()) * 100)

# create one table from the above
merged_table <- bind_rows(sex_table, age_table, ward_table, wealth_table)

# make a column of the descriptions
merged_table$characteristics <- coalesce(merged_table$sex,
                                merged_table$ward_name,
                                merged_table$age_category,
                                merged_table$wealth_index_rank)

# remove not needed columns
merged_table <- merged_table %>%
  dplyr::select(-sex, -age_category, - ward_name, - wealth_index_rank)

#reorganise the columns
desired_order <- c('characteristics', 'n', 'frequency', 'z_Score', 'stunted_percentage')

merged_table <- merged_table %>%
  dplyr::select(desired_order)

#rename the columns
merged_table <- merged_table %>%
  rename(Characteristics = characteristics,
         N = n,
         `%` = frequency,
         `Mean HAZ` = z_Score,
         `Stunted (%)` = stunted_percentage)

#make frequency and stunting rates only 2 decimal points
merged_table$`%` <- round(merged_table$`%`, 1)
merged_table$`Stunted (%)` <- round(merged_table$`Stunted (%)`, 1)
merged_table$`Mean HAZ` <- round(merged_table$`Mean HAZ`, 2)

#Print the resulting table using kbl
kbl(merged_table, caption = 'Distribution of Characteristics of Children Under Five in Mopeia') %>%
  kable_styling(bootstrap_options = c('striped', 'condensed', 
                                      font_size = 12)) %>%
  pack_rows('Sex', 1, 2) %>% # pack_rows puts the rows in groups
  pack_rows('Age Category (months)', 3, 8) %>% # in quotes is the name
  pack_rows('Ward', 9, 16) %>% #the numbers are the selected rows %>%
  pack_rows('Wealth Index Rank', 17,21) %>%
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

##############################################################################
# This will be a histogram of height 
plot_height <- ggplot(data, aes(x = height)) + 
  geom_histogram(colour = 'black') +
  labs(title = 'A.',
       x = "Height (cm)",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = -0.21))

# This will be a histogram of age 
plot_age <- ggplot(data, aes(x = age)) + 
  geom_histogram(colour = 'black') +
  labs(title = 'B.',
       x = "Age in years",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = -0.21))

# scatter plot of age and height
plot_scatter <- ggplot(data, aes(x = age,
                 y = height)) +
  geom_point(alpha = 0.7, color = 'black') +
  labs(title = 'C.',
       x = 'Age in years',
       y = 'Height (cm)') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = -0.21))
  
#all three plots in one line
combined_plots <- grid.arrange(plot_height, plot_age, plot_scatter,
                               ncol = 3) +
  theme_minimal()

#this will be a bell-curve of z scores
plot_density <- ggplot(data, aes(x = zlen)) +
  geom_density(alpha = 0.5, fill = 'darkgrey') +
  geom_vline(xintercept = -2, linetype = 'dashed', color = 'red') +
  geom_point(x = -2, y = 0.213, shape = 19, size = 2, color = "red") +
  labs(title = "D.",
       x = 'Z-score',
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = -0.21))

# plot all four plots in a 2x2 grid
top_row <- plot_grid(plot_height, plot_age, ncol = 2)
bottom_row <- plot_grid(plot_scatter, plot_density, ncol = 2)        
final_plot <- plot_grid(top_row, bottom_row, ncol = 1)
final_plot

#############################################
