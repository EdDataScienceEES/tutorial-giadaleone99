# Tutorial: Creating interactive descriptive maps in R
# Created by Giada Leone
# 14/11/2021


# clear r
rm(list= ls())

# Libraries 
library(tidyverse)
library(janitor)
library(ggthemes)
library(sf)
library(mapview)

# Load the data
beaches <- read.csv("data/beachwatch_data.csv", header = TRUE)
str(beaches)

# Preparing the data 

beaches <- beaches %>% clean_names()   # cleaning data frame column names
beaches <- select(beaches,-seq(12, 109), -seq(112, 170))  # deleting unused columns
beaches <- drop_na(beaches)  # dropping columns which have NA values

# Changing data types
beaches <- beaches %>% mutate_if(is.character, as.factor) %>% 
                       mutate_if(is.integer, as.numeric)



# Spatial analysis
beaches_obj <- st_as_sf(beaches, coords =  c('beach_longitude', 'beach_latitude'), crs = 4326)

# examine the sf objec
beaches_obj
str(beaches_obj)

plot(beaches_obj)

plot(st_geometry(beaches_obj))

# using sf object with tidyverse
filt_data <- beaches_obj %>% 
  filter(beach_region == "Scotland")
plot(filt_data$geometry)

filt_data2 <- beaches_obj %>% 
  filter(year == 2018)
plot(filt_data$geometry)

# Interactive map
mapview(beaches_obj, zcol = 'beach_region')

