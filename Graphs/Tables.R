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
library(ggplot2)
library(readr)
library(tidyverse)
library(aod)
library(rgdal)
library(ggthemes)
library(scales)
library(ggtext)

# load data
data <- read.csv("Data Files for Git/Dataset.csv")

# make a table of characteristics
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
wealth_index_rank_order <- c('Rank 1 (richest)',
                             "Rank 2",
                             "Rank 3",
                             "Rank 4",
                             "Rank 5 (poorest)")

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

#make frequency, stunting rates, and HAZ only 2 decimal points
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

# change sex names
data$sex <- factor(data$sex, 
                   levels = c("f", "m"),
                 labels = c("Female", "Male"))

# Now let's make this into a visual
sex_plot <- data %>%
  group_by(sex) %>%
  summarise(p = mean(stunted == 'TRUE'),
            ci_low = t.test(stunted == 'TRUE', 
                            conf.level = 0.95)$conf.int[1],
            ci_high = t.test(stunted == 'TRUE', 
                             conf.level = 0.95)$conf.int[2]) %>%
  ggplot(aes(x = sex,
             y = p)) +  # Add fill aesthetic for distinct bars
  geom_bar(stat = 'identity', position = 'dodge', fill = "grey80") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_text(aes(label = sex),
            position = position_stack(vjust = 0.3),
            angle = 90, hjust = 0) +
  labs(title = "A",
       x = 'Sex',  # Empty x-axis label
       y = 'Proportion Stunted') +
  scale_y_continuous(labels = percent_format(scale = 100), limits = c(0, 0.6)) +
  scale_x_discrete(labels = NULL) +  # Remove x-axis labels
  theme(axis.ticks.x = element_blank(),  # Hide x-axis ticks
        axis.title.x = element_blank(),  # Hide x-axis title
        axis.text.y = element_text(size = 10),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = -0.07))

age_plot <- data %>%
  group_by(age_category) %>%
  summarise(p = mean(stunted == 'TRUE'),
            ci_low = t.test(stunted == 'TRUE', 
                            conf.level = 0.95)$conf.int[1],
            ci_high = t.test(stunted == 'TRUE', 
                             conf.level = 0.95)$conf.int[2]) %>%
  mutate(age_category = factor(age_category, levels = age_category_order)) %>%
  ggplot(aes(x = age_category,
             y = p)) +
  geom_bar(stat = 'identity', position = 'dodge', fill = 'grey80') +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_text(aes(label = age_category),
            position = position_stack(vjust = 0.1),
            angle = 90, hjust = 0) +  # Adjust text position and angle
  labs(title = "B",
       x = 'Age category (months)',  # Empty x-axis label
       y = 'Proportion Stunted') +
  scale_y_continuous(labels = percent_format(scale = 100), limits = c(0, 0.6)) +
  scale_x_discrete(labels = NULL) +  # Remove x-axis labels
  theme(axis.ticks.x = element_blank(),  # Hide x-axis ticks
        axis.title.x = element_blank(),  # Hide x-axis title
        axis.text.y = element_text(size = 10),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = -0.07))


ward_plot <- data %>%
  group_by(ward_name) %>%
  summarise(p = mean(stunted == 'TRUE'),
            ci_low = t.test(stunted == 'TRUE', 
                            conf.level = 0.95)$conf.int[1],
            ci_high = t.test(stunted == 'TRUE', 
                             conf.level = 0.95)$conf.int[2]) %>%
  arrange(ward_name) %>%
  ggplot(aes(x = ward_name,
             y = p)) +
  geom_bar(stat = 'identity', position = 'dodge', fill = 'grey80') +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_text(aes(label = ward_name),
            position = position_stack(vjust = 0.01),
            angle = 90,  # Rotate the text by 90 degrees
            hjust = 0) +  # Adjust horizontal alignment
  labs(title = "C",
       x = 'Wards',
       y = 'Proportion Stunted') +
  scale_y_continuous(labels = percent_format(scale = 100), limits = c(0, 0.6)) +
  scale_x_discrete(labels = NULL) +
  theme(axis.text.x = element_blank(),  # Hide default x-axis labels
        axis.ticks.x = element_blank(),  # Hide x-axis ticks
        axis.title.x = element_blank(),  # Hide x-axis title
        axis.text.y = element_text(size = 10),  # Adjust y-axis text size
        panel.grid.major.x = element_blank(),  # Hide major grid lines
        panel.grid.minor.x = element_blank()) +  # Hide minor grid lines
  theme_minimal() +
  theme(plot.title = element_text(hjust = -0.07))

wealth_plot <- data %>%
  group_by(wealth_index_rank) %>%
  summarise(p = mean(stunted == 'TRUE'),
            ci_low = t.test(stunted == 'TRUE', 
                            conf.level = 0.95)$conf.int[1],
            ci_high = t.test(stunted == 'TRUE', 
                             conf.level = 0.95)$conf.int[2]) %>%
  mutate(wealth_index_rank = factor(wealth_index_rank, levels = wealth_index_rank_order)) %>%
  ggplot(aes(x = wealth_index_rank,
             y = p)) +
  geom_bar(stat = 'identity', position = 'dodge', fill = 'grey80') +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_text(aes(label = wealth_index_rank),
            position = position_stack(vjust = 0.1),
            angle = 90,  # Rotate the text by 90 degrees
            hjust = 0) +  # Adjust horizontal alignment
  labs(title = "D",
       x = 'Wealth Index Ranks',
       y = 'Proportion Stunted') +
  scale_y_continuous(labels = percent_format(scale = 100), limits = c(0, 0.6)) +
  scale_x_discrete(labels = NULL) +
  theme(axis.text.x = element_blank(),  # Hide default x-axis labels
        axis.ticks.x = element_blank(),  # Hide x-axis ticks
        axis.title.x = element_blank(),  # Hide x-axis title
        axis.text.y = element_text(size = 10),  # Adjust y-axis text size
        panel.grid.major.x = element_blank(),  # Hide major grid lines
        panel.grid.minor.x = element_blank()) +  # Hide minor grid lines
  theme_minimal() +
  theme(plot.title = element_text(hjust = -0.07))

#put all four plots in one
three_in_one <- plot_grid(sex_plot, age_plot, ward_plot, ncol = 3)
three_in_one
top_row1 <- plot_grid(sex_plot, age_plot, ncol = 2)
top_row1
bottom_row1 <- plot_grid(ward_plot, wealth_plot, ncol = 2)        
bottom_row1
final_plot <- plot_grid(top_row1, bottom_row1, ncol = 1)
final_plot

##############################################################################
#This will be a scatterplot of distance to roads and zscore
# Convert wealth_index_rank to factor
data$wealth_index_rank <- factor(data$wealth_index_rank, levels = c("Rank 1 (richest)", "Rank 2", "Rank 3", "Rank 4", "Rank 5 (poorest)"))

data$road_km <- data$road_dist / 1000

# Create a scatter plot of zscore and distance to road
ggplot(data, aes(x = road_km, y = zlen)) +
  geom_point(alpha = 0.7, color = 'black') +
  labs(title = 'C.',
       x = 'Distance to road (km)',
       y = 'Z-score') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = -0.21))

# make the scatter plot into segments to better see the distribution
data$dist_cat <- cut(data$road_km, breaks = c(0, 2, 4, 6, 8, Inf),
                     labels = c('0-2 km',
                                '2-4 km',
                                '4-6 km',
                                '6-8 km',
                                '8+ km'),
                     right = FALSE)

ggplot(data, aes(x = zlen, fill = stunted)) +
  geom_density() +
  theme_minimal() +
  facet_grid(rows = vars(dist_cat)) +
  labs(x = 'Z-score',
       y = 'Density') +
  scale_fill_manual(values = c('FALSE' = 'grey80', 'TRUE' = 'black'),
                    name = '',
                    labels = c('Not Stunted', 'Stunted')) +
  theme_minimal()

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
# creating an overlay of height and age points on the Moz growth chart image
library(grid)
library(png)
library(gridExtra)

#load image
img <- readPNG("dis/Moz_GC.png")

# create the image as a plot
image_plot <- ggplot() +
  annotation_custom(rasterGrob(img), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
image_plot <- image_plot + theme_void()

#create the plot with the points on top of the image
scatter_plot <- ggplot(data, aes(x = age, y = height)) +
  labs(x = 'Age in years', y = 'Height (cm)') +
  theme_void() +
  annotation_raster(img, xmin = 0, xmax = 5, ymin = 40, ymax = 140) +
  geom_point(data = data, aes(x = age, y = height), alpha = 0.2)

#print the image
print(scatter_plot)
