# Tutorial: Creating static and interactive descriptive maps in R
# Created by Giada Leone
# 14/11/2021


# clear r
rm(list= ls())

# Libraries 
library(tidyverse)
library(janitor)
library(leaflet)
library(ggthemes)
library(sf)
library(mapview)

# IMPORT AND CLEAN DATA ----
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

# Basic world map
ggplot() +                                # Calling ggplot()
   borders("world", colour = "black") +   # Plotting world borders in black
   theme_map()                            # Clean map theme (no axes, background or grid)
         
# Zoom in to UK 
ggplot() +
   borders("world", colour = "black") +
   coord_cartesian(xlim = c(-10, 3), ylim = c(50.3, 59)) +  # specifying coordinates to zoom
   theme_map()  

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

ggsave("outputs/beaches_map.png", width = 7, height = 5)


# CALCULATIONS, WRANGLING AND INTERACTIVE MAP ----

# Calculating beach average cigarette litter density between 2008 - 2018
# Group by beach ID, then add cig stubbs and packets then divide this 
# by the number of surveys for that beach to obtain an average 
# Then divide by 100m to obtain average density of CRL 

# Here we are going to create 2 separate data frames, so we can learn how to 
# combine them by a common column using left_join()

# Creating a dataframe with a column for total CRL
surveys <- beaches %>% 
   group_by(beach_id) %>% 
   summarise(total_crl = sum(paper_cardboard_cigarette_packets, paper_cardboard_cigarette_stubs)) %>% 
   ungroup() 

# Dataframe for number of surveys done for each beach (2008-2018)
num_surveys <- beaches %>% 
   group_by(beach_id) %>% 
   count(beach_id) %>% 
   ungroup()

# Here we are merging the two dataframes using the function left_join()
# Since the two dataframes have a column name in common, they are joined by that column
combo <- left_join(surveys, num_surveys)

# Now let's create columns for average CRL and density
# Divide the total CRL by the number of surveys done for each beach --> average
# Divide average CRL by 100 to obtain CRL density/100m
combo <- combo %>% 
   mutate(average_crl = round(total_crl/n),  # Rounding whole number
          crl_density = round(average_crl/100, 2))
   
# Creating a new dataframe with the extra information we calculated
beaches2 <- left_join(beaches, combo)



## Creating an interactive map using the leaflet package ----
# Learning how to use leaflet

leaflet() %>% addTiles() %>% 
   addCircleMarkers(data = beaches2, lat = ~beach_latitude, lng = ~beach_longitude)

# Adjusting point size
leaflet() %>% 
   addTiles() %>% 
   addCircleMarkers(data = beaches2, lat = ~beach_latitude, lng = ~beach_longitude, radius = 1)

# Making the points interactive
# Adding column to display data when points are clicked
beaches2 <- beaches2 %>% 
   mutate(popup_info = paste(beach_name, "<br/>", beach_county, "<br/>", beach_region, "<br/>", "average CRL/100m: ", average_crl ))

# Adding the popup information to the map 
leaflet() %>% 
   addTiles() %>% 
   addCircleMarkers(data = beaches2, 
                    lat = ~beach_latitude, 
                    lng = ~beach_longitude, radius = 1,
                    popup = ~popup_info)


# Colour points based on average CRL density on the beach
colours <- c("#005AB5", "#DC3200")  # Lower and upper colours of palette
                                    # These are colourblind friendly


# Creating a palette using the previously created colours object
# Palette is created for average crl_density so that beaches with high 
# CRL densities will be coloured in red and beaches with lower densities in blue
pal <- colorFactor(colours, beaches2$crl_density)


# Adding the coloured points to the map
leaflet() %>% 
   addTiles() %>% 
   addCircleMarkers(data = beaches2, 
                    lat = ~beach_latitude, 
                    lng = ~beach_longitude, radius = 1,
                    popup = ~popup_info,
                    color = ~pal(crl_density)) 
   




# sf and mapview packages ----

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




