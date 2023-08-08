# set directory
setwd("C:\\Users\\Nika\\OneDrive\\Documents\\Master's\\Dissertation\\Data Section")

# load libraries
library(dplyr)
library(readr)
library(ggplot2)
library(anthro)
library(tidyverse)
library(aod)
library(rgdal)
library(ggthemes)
library(scales)
# library(ggtext)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(sp)
library(sf)
library(dismo)
library(deldir)
library(rgeos)
library(ggrepel)

# load data
data <- read.csv("Dataset.csv")

# create a table for ward name with count and frequency while making a new column of its type
ward_table <- data %>%
  group_by(ward_name) %>%
  summarize(n = n(), frequency = n() / nrow(data))

#re-do
ward_table <- data %>%
  group_by(ward_name) %>%
  summarize(
    n = n(),
    frequency = mean(stunted_numeric)
  )

# create a table for village name with count and frequency while making a new column of its type
village_table <- data %>%
  group_by(village_name) %>%
  summarize(n = n(), frequency = n() / nrow(data))

# outline of Mopeia
load('mop.RData')
plot(mop)

#fortify the shape file so that it is ggplot compatible
mop_fortified <- fortify(mop, id = mop@data$NAME_0)

# map of stunting points on the Mopeia mop file
ggplot() +
  geom_polygon(data = mop_fortified, 
               aes(x = long,
                   y = lat,
                   group = group)) +
  geom_point(data = data,
             aes(x = lng,
                 y = lat,
                 color = stunted))

# Make choropleths (using wards) to make a better looking graph of stunting
data <- data %>% mutate(x = lng, y = lat)
cluster_points <- data %>% dplyr::select(x, y, ward_name)
coordinates(cluster_points) <- ~x+y
proj4string(cluster_points) <- proj4string(mop)
row.names(cluster_points) <- 1:nrow(cluster_points)
cluster_points$median_distance <- NA

# Remove outliers (will take a a couple of minutes)
for(i in 1:nrow(cluster_points)){
  this_point <- cluster_points[i,]
  other_points_in_ward <- cluster_points[cluster_points$ward_name == this_point$ward_name,]
  distances <- rgeos::gDistance(this_point, other_points_in_ward, byid = TRUE)
  median_distance <- median(distances)
  cluster_points$median_distance[i] <- median_distance
}

# Remove those with large median distance
cluster_points <- cluster_points[cluster_points$median_distance <= 0.2,]

# voronoi tesselation
v <- dismo::voronoi(cluster_points)
# inspect
plot(v)
# collapse
x = gUnaryUnion(v, id = v$ward_name, checkValidity = 2)
# match row names
ward_table <- data.frame(ward_table)
row.names(ward_table) <- as.character(ward_table$ward_name)
# make polygons dataframe
dfv <- SpatialPolygonsDataFrame(Sr = x, data = ward_table, match.ID = TRUE)
# inspect data
dfv@data
# reproject
proj4string(dfv) <- proj4string(mop)
# clip
r <- gIntersection(mop, dfv, byid = TRUE)
r <- SpatialPolygonsDataFrame(Sr = r, data = ward_table, match.ID = FALSE)
# Fortify
rf <- fortify(r, regions = r@data$ward_name)
# clean up
rf$ward_name <- gsub('30 ', '', rf$id)
# join with ward info
rf <- left_join(rf, ward_table)

# Calculate the centroids of each choropleth polygon
rf_centroids <- rf %>%
  group_by(ward_name) %>%
  summarize(x = median(long),
            y = median(lat),
            x1 = mean(long),
            y1 = mean(lat))

# Function to add line breaks at periods in ward names
wrap_ward_names <- function(x) {
  str_wrap(x, width = 14)  # Adjust the width value as needed to control the length of each line
}

# plot of prevalence of stunting in children under 5 in Wards
ggplot() + # add polygon layers from 'rf' dataset
  geom_polygon(data = rf,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = frequency),
               color = 'white') + 
  # define the fill color gradient and customize the legend
  scale_fill_gradient2_tableau(name = 'Prevalence of Stunting',
                               palette = 'Green-Blue Diverging',
                               labels = label_percent(scale = 100)) + 
  # add ward name labels using repelling to prevent overlaps
  geom_label_repel(data = rf_centroids,
                   aes(x = x, 
                       y = y, 
                       label = ward_name),
                   size = 3.5, color = "black", fontface = "bold") +
  #apply the 'theme_map' theme for the plot
  ggthemes::theme_map() +
  # Adjust the position of the legend
  theme(legend.position = c(0.05,-0.15)) +
  # Increase the plot margins to create more space for ward name labels
  theme(plot.margin = margin(0, 0, 0, 0, "cm")) + 
  # Increase the spacing between the legend and the plot
  theme(legend.spacing = unit(0, "lines")) +
  # Customize the appearance of the title
  theme(plot.title = element_text(size = 14, 
                                  face = "bold", 
                                  family = "Arial", 
                                  color = "darkblue",
                                  hjust = 0,
                                  vjust = 0,
                                  margin = margin(b = 0))) + #the space between title and map
  # Rotate and position the stunting legend vertically at the bottom
  guides(fill = guide_colorbar(title.position = "top", 
                               title.hjust = 0.5, 
                               title.vjust = 0.5,
                               direction = "horizontal")) +
  # Set the aspect ratio to control squishing
  coord_fixed() +
  # Add a title to the plot
  labs(title = " Prevalence of Stunting in Children Under 5 in Wards")

#####################  

# make a table of each ward and the number of kids stunted and not stunted
data %>%
  group_by(ward_name) %>%
  summarize(stunted_count = sum(stunted),
            not_stunted = sum(!stunted))

data %>% 
  summarize(stunted_count = sum(stunted),
            not_stunted = sum(!stunted))