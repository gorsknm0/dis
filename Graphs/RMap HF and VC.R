#plot of prevalence of sunting in children under 5 with the health facility points and center of town

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
data <- read.csv("Data Files for Git/Dataset.csv")

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
load('dis/mop.RData')
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

# Read in all health facilities
hf_1 <- st_read("tmp/crf/health_facilities/health_facilities.shp")

# Convert sf object to a dataframe
hf_1 <- as.data.frame(hf_1)

# Add a new column 'Type' to hf_1 with value 'Health Facility'
hf_1$Type <- "Health Facility"

# Select specific columns in hf_1 (lng, lat, Type)
hf_1 <- dplyr::select(hf_1, lng, lat, Type)

# Point data for the center of village
x_coord <- 35.710798061452984
y_coord <- -17.981304088784032

# Create a data frame with the x and y coordinates and 'Type' as 'Village Center'
point_data <- data.frame(lng = x_coord, 
                         lat = y_coord,
                         Type = "Village Center")

# Combine hf_1 and point_data into a single data frame
combined_data <- rbind(hf_1, point_data)

# create a plot of sunting with the health facility and village center points added
ggplot() +
  # add polygon layers from 'rf' dataset
  geom_polygon(data = rf,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = frequency),
               color = 'white') + 
  # define the fill color gradient and customize the legend
  scale_fill_gradient2_tableau(name = 'Prevalence of\nstunting',
                               palette = 'Green-Blue Diverging',
                               labels = label_percent(scale = 100)) +
  # Reproject 'point_data' to the CRS of 'hf_1' and then combine them into a single data frame
  geom_point(data = combined_data,
             aes(x = lng, 
                 y = lat, 
                 color = Type,
                 shape = Type), 
             size = 3, 
             alpha = 1, 
             show.legend = TRUE) +
  # Customize point colors and legend title
  scale_color_manual(values = c("black", "red"),
                     labels = c("Health Facility", "Village Center"),
                     name = "Point Types") +
  scale_shape_manual(values = c("Health Facility" = 5, 
                                "Village Center" = 16),
                     labels = c("Health Facility",
                                "Village Center"),
                     name = "Point Types") +
  # Apply a theme for the map
  theme_map() +
  # Adjust the position of the legend
  theme(legend.position = c(1,-0.1)) +
  # Increase the plot margins to create more space for ward name labels
  theme(plot.margin = margin(0, 3, 0, 0, "cm")) +
  # Increase the spacing between the legend and the plot
  theme(legend.spacing = unit(0, "lines")) +
  # Customize the appearance of the title
  theme(plot.title = element_text(size = 14, 
                                  face = "bold", 
                                  family = "Arial", 
                                  color = "darkblue",
                                  hjust = 0,
                                  vjust = 0,
                                  margin = margin(b = 20))) + 
  # Set the aspect ratio to control squishing
  coord_fixed() +
  # Add a title to the plot
  labs(title = "Prevalence of Stunting in Children Under 5 in Relation\nto Health Facilities and the Village Center")
