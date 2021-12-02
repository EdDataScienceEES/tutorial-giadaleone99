# Tutorial: Creating static and interactive descriptive maps in R
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


# CREATING A STATIC MAP ----

# Basic map
ggplot() +
   borders("world", colour = "black") +
   theme_bw()

# Zoom in to UK 
ggplot() +
   borders("world", colour = "black") +
   coord_cartesian(xlim = c(-10, 3), ylim = c(50.3, 59)) +  # specifying coordinates to zoom
   theme_map()  # Gets rid of default long and lat axes and grids

# Adding the surveyed beaches within the aes() function
ggplot(beaches, aes(x = beach_longitude,
                    y = beach_latitude)) +
   borders("world", colour = "black", fill = "lightgrey") +  # changing the colour of the map
   coord_cartesian(xlim = c(-10, 3), ylim = c(50.3, 59)) +
   geom_point(size = 1) +
   theme_map() 
   

# Changing map background
ggplot(beaches, aes(x = beach_longitude,
                    y = beach_latitude)) +
   borders("world", colour = "black", fill = "lightgrey") +  
   coord_cartesian(xlim = c(-10, 3), ylim = c(50.3, 59)) +
   geom_point(size = 1) +
   theme_map() +
   theme(panel.background = element_rect(fill = "aliceblue"))

# Want to change the point colour according to region?
ggplot(beaches, aes(x = beach_longitude,
                    y = beach_latitude,
                    colour = beach_region)) +
   borders("world", colour = "black", fill = "lightgrey") +  
   coord_cartesian(xlim = c(-10, 3), ylim = c(50.3, 59)) +
   geom_point(size = 1) +
   theme_map() +
   theme(panel.background = element_rect(fill = "aliceblue"))

# Let's move the legend so that it is not overlapping with the map
# Here we also change the legend title and add a legend box to make it stand out
ggplot(beaches, aes(x = beach_longitude,
                    y = beach_latitude,
                    colour = beach_region)) +
   borders("world", colour = "black", fill = "lightgrey") +  
   coord_cartesian(xlim = c(-10, 3), ylim = c(50.3, 59)) +
   geom_point(size = 1) +
   theme_map() +
   theme(plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'),  # title characteristics
         panel.background = element_rect(fill = "aliceblue"),
         legend.position = c(0.77, 0.45),
         legend.box.background = element_rect(color = 'grey', size = 0.5)) +  # Adding a border
   labs(colour = "UK Regions",  # Changing legend title
        title = "Great British Beach Clean surveys in the UK")  # Informative title


(map1 <- ggplot(beaches,
               aes(x = beach_longitude,
                   y = beach_latitude, 
                   colour = beach_region)) +
   borders("world", colour = "grey5", fill = "black", size = 0.2) +
   coord_cartesian(xlim = c(-10, 3), ylim = c(50.3, 59)) +
   theme_map() +
   geom_point(size = 1) +
   theme(plot.title = element_text(size = 12, hjust = 0.5),
         legend.position = c(0.77, 0.4),
         legend.box.background = element_rect(color = 'grey', size = 0.5)) +
   labs(title = "Beach Locations by Region",
        colour = "UK Regions"))



ggsave("outputs/beaches_map.png", width = 7, height = 5)



# Calculating beach cigarette litter density in 2018
# Group by beach ID, then add cig stubbs and packets then divide this 
# by the number of surveys*100m? 

# Here we are going to create 2 separate data frames, so we can learn how to 
# combine them by a common column using left_join()

# Filtering for 2018 surveys only
surveys_2018 <- beaches %>% 
   filter(year %in% 2018) %>% 
   group_by(beach_id) %>% 
   summarise(total_crl = sum(paper_cardboard_cigarette_packets, paper_cardboard_cigarette_stubs)) %>%
ungroup() 

# So we have the total cigarette related litter counts for all beaches in 2018
# Now we need to divide that by the number of surveys done for each beach ID

num_surveys <- beaches %>% 
   filter(year %in% 2018) %>% 
   group_by(beach_id) %>% 
   count(beach_id) %>% 
ungroup()

# Combining the datasets
combo <- left_join(surveys_2018, num_surveys)

# Now let's divide the total crl by the number of surveys done for each beach
combo <- combo %>% 
   mutate(average_crl = round(total_crl/n),
          crl_density = round(average_crl/100, 2))

# Now we can create a 


   
   





## Creating an interactive map using the leaflet package ----
# Learining how to use leaflet
install.packages("leaflet")
library(leaflet)

leaflet() %>% addTiles() %>% 
   addCircleMarkers(data = beaches, lat = ~beach_latitude, lng = ~beach_longitude)



# Adjusting point size
leaflet() %>% 
   addTiles() %>% 
   addCircleMarkers(data = beaches, lat = ~beach_latitude, lng = ~beach_longitude, radius = 1)


# Making the points interactive
# Adding data to display when points are clicked


# Going to create a new column for the popup information in the beaches dataset 
# Possibly add density of cigarette related litter?


beaches <- beaches %>% 
   mutate(popup_info = paste(beach_name, "<br/>", beach_county, "<br/>", beach_region))


# Adding the popup information to the map 
leaflet() %>% 
   addTiles() %>% 
   addCircleMarkers(data = beaches, 
                    lat = ~beach_latitude, 
                    lng = ~beach_longitude, radius = 1,
                    popup = ~popup_info)


# Create a palette to colour points by cigarette related litter density

colours























# sf and mapview packages

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




