# -----------------------------------------------------------
# Script Name: SD East/West River County Names
# Description:This script loads geometries for South Dakota counties
# and uses their bounding boxes to determine if they are east or west
# of the Missouri river.  Sourcing this script creates a variable
# — sd_counties_east_west — with this dataframe
# -> a csv of South Dakota counties and whether they are east/west river
# Author: Slater Dixon
# Date: Jan 9 2023
# Version: 1.0
# -----------------------------------------------------------

library(tidyverse)
library(tigris)
library(janitor)


get.county.sides <- function(clean.df.names = T){
  # Get county shapes from tigris
  east.west <- counties("SD") |> 
    # Calculate bounding box for each county
    mutate(bbox = lapply(sd.counties$geometry, st_bbox)) |> 
    # Unnest into xmin, xmax, ymin, ymax columns
    unnest_wider(bbox) |>
    # Create east_west categorical
    mutate(east_west = case_when(
      # Set northeast and south east as East River
      (xmax > -99.78 & ymax > 44.47) | (xmax > -98.4 & ymax < 44.7) ~ "East River",
      # Set others as West River
      (xmax < -99) ~ "West River",
      
      # Manually set border counties
      grepl("Buffalo", NAME) | grepl("Brule", NAME) ~ "East River",
      NAME %in% c("Gregory") ~ "West River"
    )) 
  
  if(clean.df.names){
    east.west <- east.west |>
      select(COUNTYFP, NAME, NAMELSAD, east_west) |>
      clean_names() 
  }
  
  return(east.west)
}

plot.counties <- function(county.sides){
  county.sides |>
    st_sf() |>
    ggplot() +
    geom_sf(aes(fill = east_west)) +
    geom_sf_label(aes(label = NAME), label.size = .1) +
    theme_void() 
    
}

write.counties <- function(county.sides){
  county.sides |>
    select(COUNTYFP, COUNTYNS, GEOID, NAME, NAMELSAD, east_west) |>
    clean_names() |>
    write_csv("sd_county_river_affs.csv")
}


sd_counties_east_west <- (get.county.sides)
