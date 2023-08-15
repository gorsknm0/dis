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


model <- lmer(stunted_numeric ~ rich_or_poor + admin_km + (1 | cluster), data = data)

# Assign 0 to NA values in a specific column (e.g., column1)
data$hh_n_cows[is.na(data$hh_n_cows)] <- 0
data$hh_n_pigs[is.na(data$hh_n_pigs)] <- 0

model <- lmer(stunted_numeric ~ sex + road_km + hh_member_num + hh_head_age + hh_head_gender + hh_n_constructions + n_nets_in_hh + member_per_sleep + animals + hh_main_building_type + hh_main_wall_material + cook_main_water_source + cook_time_to_water + hh_main_energy_source_for_lighting + hh_possession_Radio + hh_possession_TV + hh_possession_Cell_phone + (1 | cluster), data = data)

model <- lmer(stunted_numeric ~ sex + age + road_km + hh_member_num + hh_head_age + hh_head_gender + hh_n_constructions + n_nets_in_hh + member_per_sleep + animals + building_type_apartment + building_type_conventional_house + building_type_flat + building_type_hut + building_type_precarious + building_type_traditional_mud_house + hh_main_wall_material + cook_main_water_source + cook_time_to_water + lighting_energy_electricity + lighting_energy_generator + lighting_energy_solar_panel + lighting_energy_gas + lighting_energy_oil + lighting_energy_candles + lighting_energy_batteries + lighting_energy_firewood + hh_possession_Radio + hh_possession_TV + hh_possession_Cell_phone + (1 | cluster), data = data)

tab_model(model)

#model <- lmer(stunted_numeric ~ sex + hh_member_num + hh_head_age + hh_head_gender + road_km + hh_n_constructions + n_nets_in_hh + member_per_sleep + animals + building_type_apartment + building_type_conventional_house + building_type_flat + building_type_hut + building_type_precarious + building_type_traditional_mud_house + hh_wall_adobe_block + hh_wall_bamboo + hh_wall_tin + hh_wall_bark + hh_wall_tinned_wood + hh_wall_palm_tree + hh_wall_brick_block + water_source_piped_water_house + water_source_piped_water_compound + water_source_piped_water_neighbor + water_source_fountain + water_source_protected_well_in_backyard + water_source_protected_well_out_backyard + water_sourcel_unprotected_well_in_household + water_source_unprotected_well_out_household + water_source_hole_man_pump_inside_household + water_source_hole_protected_hand_pump_yard + water_source_surface + water_source_rainwater + water_source_mineral_bottled_water + water_source_water_tank_truck + water_time_under_10_min + water_time_between_10_30_min + water_time_between_30_60_min + water_time_more_than_hour + lighting_energy_electricity + lighting_energy_generator + lighting_energy_solar_panel + lighting_energy_gas + lighting_energy_oil + lighting_energy_candles + lighting_energy_batteries + lighting_energy_firewood + hh_possession_Radio + hh_possession_TV + hh_possession_Cell_phone + any_deaths_past_year + (1 | cluster), data = data)

# hh_n_constructions + n_nets_in_hh + member_per_sleep + hh_n_cows + hh_n_pigs, animals + hh_main_building_type + hh_main_wall_material + cook_main_water_source + cook_time_to_water + hh_main_energy_source_for_lighting + hh_possession_Radio + hh_possession_TV + hh_possession_Cell_phone + any_deaths_past_year + hh_head_age + hh_head_gender
