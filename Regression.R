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

#Since no difference in quintiles was recorded in the ranks of wealth index, create a variable of wealthiest and others
data$rich_or_poor <- ifelse(data$wealth_index_rank == 'Rank 1 (richest)',
                            'rich', 'poor')

data$rich_or_poor <- factor(data$rich_or_poor, levels = c('rich', 'poor'))

mylogit <- glm(stunted_numeric ~ rich_or_poor + roster_size + age_in_months + cluster + road_dist + total_vax, 
               data = new, 
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

new <- data %>% 
  drop_na(total_vax)

library(lme4)
model <- lmer(zlen ~ roster_size + rich_or_poor + road_dist + (1 | cluster), data = data)

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

##################### NOT SURE THIS IS NEEDED ##############################

# obtain confidence intervals for the coefficient estimate
#CIs using profiles log-likelihood
confint(mylogit)

#CIs using standard errors
confint.default(mylogit)

# test for an overall effect of wealth_index_rank
wald.test(b = coef(mylogit), 
          Sigma = vcov(mylogit), 
          Terms = 2:4) # terms is associated with the terms found when run in the above line with '(Intercept)' being the first term

# testing difference in coefficients (rank 5 and 1)
# create a vector to define the test [difference between rank =2 and rank =5]
l <- cbind(0, 1, 0, 0, -1, 0, 0, 0) #hold on may have realised not all my ranks show for wealth
wald.test(b = coef(mylogit), 
          Sigma = vcov(mylogit),
          L = l)

#odds ratios only
exp(coef(mylogit))

#odds ratios and 95% CI
exp(cbind(OR = coef(mylogit),
          confint(mylogit)))

#calculate stunted probability at each value of rank, holding others at their means
# first, create and view dataframe
newdata1 <- with(data, 
                 data.frame(admin_dist = mean(admin_dist),
                            health_dist = mean(health_dist),
                            road_dist = mean(road_dist),
                            wealth_index_rank = levels(data$wealth_index_rank)))

#The first line of code below is quite compact, we will break it apart to discuss what various components do. 
#The newdata1$rankP tells R that we want to create a new variable in the dataset (data frame) newdata1 called rankP, the rest of the command tells R that the values of rankP should be predictions made using the predict( ) function. 
#The options within the parentheses tell R that the predictions should be based on the analysis mylogit with values of the predictor variables coming from newdata1 and that the type of prediction is a predicted probability (type="response"). 
#The second line of the code lists the values in the data frame newdata1. Although not particularly pretty, this is a table of predicted probabilities.
newdata1$wealth_index_rankP <- predict(mylogit,
                          newdata = newdata1, 
                          type = "response")

#create a table of predicted probabilities varying the value of admin_dist and wealth_index_rank
newdata2 <- with(data, data.frame(admin_dist = rep(seq(from = 22, to = 97973, length.out = 100), ), health_dist = mean(health_dist), road_dist = mean(road_dist), wealth_index_rank = levels(rep(data$wealth_index_rank))))

#generate the predicted probabilities and ask for standard error to plot a confidence interval
newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                    se = TRUE))

newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

ggplot(newdata3, 
       aes(x = admin_dist, 
           y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL,
                  ymax = UL, 
                  fill = wealth_index_rank), 
              alpha = 0.2) + 
  geom_line(aes(colour = wealth_index_rank),
            size = 1)

##########################new##########

#create a histogram for admin_dist
ggplot(data,
       aes(x=admin_dist)) +
  geom_histogram(binwidth = 3000)

cor(df)

#create a boxplot for admin_dist
ggplot(data, aes(x = "", y = admin_dist)) +
  geom_boxplot()

#scatter plot of stunting and admin_dist
ggplot(data,
       aes(x = wealth_index_rank,
           y = zlen)) +
  geom_point() 

#creating boxplots for the variables
plot1 <- ggplot(data, aes(x = factor(stunted),
                          y = admin_dist)) +
  geom_boxplot() 

plot2 <- ggplot(data, aes(x = factor(stunted),
                          y = health_dist)) +
  geom_boxplot() 

plot3 <- ggplot(data, aes(x = factor(stunted),
                          y = road_dist)) +
  geom_boxplot() 

#put plots on same grid
library(gridExtra)

grid.arrange(plot1, plot2, plot3, ncol = 3)

#############TESTING###################

#generate a correlation matrix
library(caret)

cor_data <- data[, c("admin_dist", "health_dist", "road_dist", "zlen")]

# Calculate the correlation matrix
cor_matrix <- cor(cor_data)

# Print the correlation matrix
print(cor_matrix)

library(reshape2)
heatmap_data <- melt(cor_matrix)  # Convert the correlation matrix to long format
ggplot(heatmap_data, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "blue") +
  labs(title = "Correlation Heatmap",
       x = "Variables",
       y = "Variables")
