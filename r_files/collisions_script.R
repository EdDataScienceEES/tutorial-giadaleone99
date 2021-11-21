## Option 2 


# clear r
rm(list= ls())

# Libraries 
library(tidyverse)
library(janitor)

# Importing dataset
collision_data <- read.csv("data/collision_data.csv")

view(collision_data)  # viewing dataset 

unique(collision_data$Country)  # Let's focus on Europe

colnames(collision_data)

Europe_collisions <- collision_data %>%
  filter(Country %in% c("Portugal", "Netherlands", "Austria", "Spain", "Germany", "Poland", "UK", "Norway", "Greece", "Belgium", "Italy", "Switzerland", "France")) %>% 
  clean_names() %>% 
  drop_na(lat) %>% 
  drop_na(long)

Europe_collisions$lat <- as.double(Europe_collisions$lat)

Europe_collisions[!is.na(Europe_collisions$Lat),
                  !is.na(Europe_collisions$Long),]

df[!is.na(df$Col3),]

UK_collisions <- collision_data %>% 
  filter(Country %in% "UK") %>% 
  clean_names() 


# Figure out how to drop rows with NAs in 2 different columns





glimpse(UK_collisions)
UK_collisions$lat <- as.double(UK_collisions$lat)

glimpse(Europe_collisions)

collisions_obj <- st_as_sf(Europe_collisions, coords =  c('long', 'lat'), crs = 4326)

plot(collisions_obj)

mapview(collisions_obj, zcol = 'country')
