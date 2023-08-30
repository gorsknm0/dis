# set directory
setwd("C:\\Users\\Nika\\OneDrive\\Documents\\Master's\\Dissertation\\Data Section")

# load libraries
library(aod)
library(ggplot2)
library(dplyr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(lme4)
library(tidyverse)

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

#this is a logistic regression (not used in this research)
mylogit <- glm(stunted_numeric ~ rich_or_poor + cluster + road_dist, 
               data = data, 
               family = 'binomial')

summary(mylogit)

ors <- exp(coef(mylogit))
cis <- exp(confint(mylogit))
ors <- data.frame(ors)
ors$lwr <- cis[,1]
ors$upr <- cis[,2]


#make a table of the findings
tab_model(mylogit,
          #pred.labels = c("Intercept", "Poor Individuals", "Distance from Administrative Posts", "Household Roster Size", "Distance from Nearest Health Facility"),
          dv.labels = "Model of Stunting",
          string.ci = "Conf. Int (95%)",
          string.p = "P-Value"
          #p.style = "stars"
          )

# how do clusters near or far from the health center impact stunting?
# create df where we can see statistics of stunting in cluster, calculate centroid of cluster to health facility, cluster, percentage of stunting, average distance to health facility, fit a logit model to that. 

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

model <- lmer(stunted_numeric ~ wealth_index_rank + road_km + (1 | cluster), data = data)

summary(model)

tab_model(model,
         # pred.labels = c("Intercept", "Distance from Nearest Health Facility", "No. of People in Household", "Poor Category"),
          dv.labels = "Model of Stunting",
          string.ci = "Conf. Int (95%)",
          string.p = "P-Value")
# this is where you can find information about tab_model
# https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_model_estimates.html


# Perform linear regression
model_lm <- lm(zlen ~ wealth_index_rank + road_km, 
            data = data)

tab_model(model_lm)

# Assign 0 to NA values in a specific column (e.g., column1)
data$hh_n_cows[is.na(data$hh_n_cows)] <- 0
data$hh_n_pigs[is.na(data$hh_n_pigs)] <- 0

# model looking at the entire wealth index
model <- lmer(stunted_numeric ~ rich_or_poor + road_km + (1 | cluster), data = data)
tab_model(model)

# model looking at stunting and the variables of wealth used in the index
real_deal <- lmer(stunted_numeric ~ sex + age + road_km + hh_member_num + hh_head_age + hh_n_constructions + building_type_hut + building_type_precarious + building_type_traditional_mud_house +  cook_main_water_source + cook_time_to_water + lighting_energy_electricity + lighting_energy_generator + lighting_energy_solar_panel + lighting_energy_gas + lighting_energy_oil + lighting_energy_candles + lighting_energy_batteries + lighting_energy_firewood + hh_possession_Radio + hh_possession_TV + hh_possession_Cell_phone + hh_head_gender + member_per_sleep + n_nets_in_hh + animals + building_type_apartment + building_type_conventional_house + building_type_flat + hh_wall_adobe_block + hh_wall_bamboo + hh_wall_tin + hh_wall_bark + hh_wall_tinned_wood + hh_wall_palm_tree + hh_wall_brick_block + (1 | cluster), data = data)

# both models tables in one table
tab_model(real_deal, model,
          pred.labels = c('Intercept', 'Sex (male)', 'Age (years)', 'Distance to road (km)', 'Building type - hut', 'Building type - precarious', 'Building type - traditional mud house', 'Main water source - hole with manual pump inside house', 'Main water source - hole protected with hand pump outside backyard', 'Main water source for consumption- lagoon', 'Main water source for consumption - lake', 'Main water source for consumption - piped water within compound', 'Main water source for consumption - piped water inside house', 'Main water source for consumption - piped water in neighbour house', 'Main water source for consumption - protected inside backyard','Main water source for consumption - protected outside backyard', 'Main water source for consumption - rainwater', 'Main water source for consumption - unprotected inside household', 'Main water source for consumption - unprotected outside household', 'Main water source for consumption - river', 'Time for taking main water sources - between 30 & 60 min', 'Time for taking main water sources - more than hour', 'Time for taking main water sources - under 10 min', 'Main energy source for lighting - generator', 'Main energy source for lighting - solar panel', 'Main energy source for lighting - gas', 'Main energy source for lighting - candles', 'Main energy source for lighting - batteries', 'Main energy source for lighting - firewood', 'Ownership of Radio', 'Ownership of TV', 'Ownership of cell phone', 'HH gender (male)', 'Number of household members', 'HH age', 'Constructions', 'Members per sleeping room', 'Bed nets', 'Animals - no livestock', 'Animals - pigs', 'Animals - pigs and cows', 'Animals - unknown', 'Building type - apartment', 'Building type - conventional house', 'Building type - flat', 'Building type - traditional mud house', 'Wall material in main house - adobe block', 'Wall material in main house - bamboo', 'Wall material in main house - tin', 'Wall material in main house - bark', 'Wall material in main house - tinned wood', 'Wall material in main house - palm tree', 'Wall material in main house - brick block', 'Wealth Index Rank 1 or Other (Other)'),
          dv.labels = c("First Model", "Second Model"),
          string.pred = "Variables",
          string.ci = "Conf. Int (95%)",
          string.p = "P-Value")
