# set the directory
setwd("C:\\Users\\Nika\\OneDrive\\Documents\\Master's\\Dissertation")

#load libraries
library(ggplot2)
library(dplyr)
library(readr)
library(anthro)
library(tidyverse)
library(aod)
library(rgdal)
library(ggthemes)
library(scales)
library(ggtext)

#load the data
data <- read_csv('Data Section/Dataset.csv')

# Make age categories in the proper order
age_category_order <- c('0-5', '6-11', '12-23', '24-35', '36-47', '48-60')

# the national average of stunting according to USAID
national_average <- 0.43

# the average of stunting in Mopeia according to this data
mopeia_average <- mean(data$stunted)

# create a graph that shows the proportion of children stunted by age group
data %>%
  group_by(age_category) %>%
  summarise(p = mean(stunted == 'TRUE'),
            ci_low = t.test(stunted == 'TRUE', 
                            conf.level = 0.95)$conf.int[1], # compute lower bound of ci
            ci_high = t.test(stunted == 'TRUE', 
                             conf.level = 0.95)$conf.int[2]) %>% #compute upper bound of CI
  mutate(age_category = factor(age_category, levels = age_category_order)) %>% #puts the x axis in the right order
  ggplot(aes(x = age_category,
             y = p)) +
  geom_bar(stat = 'identity', position = 'dodge', fill = 'grey80') +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                position = position_dodge(width = 0.9), width = 0.2) + # add confidence intervals
  geom_hline(yintercept = national_average, linetype = "dashed", color = "darkblue") + 
  # add mopeia average line
  geom_hline(yintercept = mopeia_average, linetype = "dashed", color = "darkred") + 
  # add horizontal line
  geom_text(aes(x = 3.15, y = national_average + 0.02,
                label = "National Stunting Average of 43% (Source: USAID)"),
            # make words bold fontface = 'bold',
            vjust = 0, hjust = 0, color = "darkblue", angle = 0, size = 3) + 
  #label the mopeia line
  geom_text(aes(x = 4.8, y = mopeia_average - 0.05,
               label = "Mopeia Average of 81%"),
           vjust = 0, hjust = 0, color = "darkred", angle = 0, size = 3) +
  # add text annotation
  labs(title = 'Proportion of Children Stunted by Age Group',
       x = 'Age in Months',
       y = 'Proportion of Stunted Children') +
  scale_y_continuous(labels = percent_format(scale = 100), limits = c(0, 1)) +
  theme_minimal()
