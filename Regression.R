# set directory
setwd("C:\\Users\\Nika\\OneDrive\\Documents\\Master's\\Dissertation\\Data Section")

# load libraries
library(aod)
library(ggplot2)
library(dplyr)

# load data
data <- read.csv("Data Files for Git/Dataset.csv")

#two-way contingency table of categorical outcome and predictors we want to make sure there are not 0 cells
xtabs(~stunted_numeric + wealth_index_rank, data = data)

# convert wealth_index_rank to a categorical variable (factor)
data$wealth_index_rank <- factor(data$wealth_index_rank)

#make th distance from meters to kilometers
data$admin_km <- data$admin_dist / 1000
data$health_km <- data$health_dist / 1000
data$road_km <- data$road_dist / 1000

#Since no difference in quintiles was recorded in the ranks of wealth index (regression), 
# create a variable of wealthiest and others
data$rich_or_poor <- ifelse(data$wealth_index_rank == 'Rank 1 (richest)',
                            'rich', 'poor')

data$rich_or_poor <- factor(data$rich_or_poor, levels = c('rich', 'poor'))

mylogit <- glm(stunted_numeric ~ rich_or_poor + roster_size + cluster + road_dist, 
               data = data, 
               family = 'binomial')

# rich_or_poor + admin_dist + roster_size + health_km ( insert for mylogit )

summary(mylogit)

ors <- exp(coef(mylogit))
cis <- exp(confint(mylogit))
ors <- data.frame(ors)
ors$lwr <- cis[,1]
ors$upr <- cis[,2]




############testing to make a table
# load package
library(sjPlot)
library(sjmisc)
library(sjlabelled)

tab_model(mylogit,
          #pred.labels = c("Intercept", "Poor Individuals", "Distance from Administrative Posts", "Household Roster Size", "Distance from Nearest Health Facility"),
          dv.labels = "Model of Stunting",
          string.ci = "Conf. Int (95%)",
          string.p = "P-Value"
          #p.style = "stars"
          )

# clusters near or far health center how does it impact stunting?
#create dataset where see statistic of stunting in cluster, calculate centroid of cluster to health facility. cluster, percentage of stunting, average distance to health facility, fit a logit model to that. 

test <- data %>%
  group_by(cluster) %>%
  summarise(stunted_count = sum(stunted),
            not_stunted = sum(!stunted),
            average_dist = median(health_km)) %>%
  mutate(percent_stunted = stunted_count / (not_stunted + stunted_count))

summary(glm(percent_stunted~average_dist, family = binomial, data = test))

#fit multilevel model with a random effect for the individuals
#package lme4 with cluster as the fixed effect
#clustered logistic regression

library(lme4)
model <- lmer(stunted_numeric ~ rich_or_poor + admin_km + (1 | cluster), data = data)

summary(model)

tab_model(model,
         # pred.labels = c("Intercept", "Distance from Nearest Health Facility", "No. of People in Household", "Poor Category"),
          dv.labels = "Model of Stunting",
          string.ci = "Conf. Int (95%)",
          string.p = "P-Value")

#using zlen for a model
library(tidyverse)

# Perform linear regression
model1 <- lm(zlen ~ rich_or_poor + roster_size + cluster + road_dist, 
            data = data)

tab_model(model1)

# https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_model_estimates.html

#testing with new variables
#sex, hh_size, hh_sub_size, hh_head_age, hh_head_gender, hh_member_num_residents, lone_resident_households, hh_n_constructions, hh_n_constructions_sleep, hh_type, cook_water_source, time_to_cook_water, main_source_energy_for_lighting, hh_n_cows, hh_n_pigs, animals,  hh_wall_adobe_block, hh_wall_bamboo, hh_wall_brick_block, hh_wall_wood, hh_wall_palm_tree, hh_wall_tin, hh_wall_tinned_wood, hh_wall_bark, hh_wall_Other, hh_possession_Radio, hh_possession_TV, hh_possession_Cell_phone, irs_past_12_months, n_nets_in_hh, any_deaths_past_year


  # Drop rows containing NAs from specified columns
  testing_data <- data %>% data[complete.cases(data$hh_size, data$hh_sub_size, data$hh_head_age, data$hh_head_gender, data$hh_member_num_residents, data$lone_resident_households, data$hh_n_constructions, data$hh_n_constructions_sleep, data$hh_type, data$cook_water_source, data$time_to_cook_water, data$main_source_energy_for_lighting, data$hh_n_cows, data$hh_n_pigs, data$animals,  data$hh_wall_adobe_block, data$hh_wall_bamboo, data$hh_wall_brick_block, data$hh_wall_wood, data$hh_wall_palm_tree, data$hh_wall_tin, data$hh_wall_tinned_wood, data$hh_wall_bark, data$hh_wall_Other, data$hh_possession_Radio, data$hh_possession_TV, data$hh_possession_Cell_phone, data$irs_past_12_months, data$n_nets_in_hh, data$any_deaths_past_year), ]

model <- lmer(stunted_numeric ~ sex + hh_size + hh_sub_size + hh_head_age + hh_head_gender + hh_member_num_residents + lone_resident_households + hh_n_constructions + hh_n_constructions_sleep + hh_type + cook_water_source + time_to_cook_water + main_source_energy_for_lighting + hh_n_cows + hh_n_pigs + animals +  hh_wall_adobe_block + hh_wall_bamboo + hh_wall_brick_block + hh_wall_wood + hh_wall_palm_tree + hh_wall_tin + hh_wall_tinned_wood + hh_wall_bark + hh_wall_Other + hh_possession_Radio + hh_possession_TV + hh_possession_Cell_phone + irs_past_12_months + n_nets_in_hh + any_deaths_past_year + admin_km + (1 | cluster), data = testing_data)
