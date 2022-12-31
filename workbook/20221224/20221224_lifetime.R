############################## Lifetime Data ###################################
# 
# Analyses in this script:
#   -Use Alex's functions to read in, isolate, and clean lifetime music
#   -Try to map ip addresses
# 
################################################################################

library(rgeolocate)
library(plotly)

#### using Alex's functions to import lifetime data ####
# read in Alex's functions
source('workbook/20221222/alex_fxns.R')

life0 <- import_lifetime_streaming_data('data/ignored_data')

life <- get_lifetime_tracks(life0)

head(life)

#### mapping IP addresses ####
# NOTE: edited Alex's function to include IP address

library(data.table)
library(R.utils)
library(tidyverse)
library(plotly)


# reading in ip address database
system.time(
  rgeolocate::maxmind(
    life$ipAddress, "data/ignored_data/dbip-city-lite-2022-12.mmdb", c("longitude", "latitude")
  ) -> xdf
)


# bind latitude and longitude to main dataset
life$latitude <- xdf$latitude
life$longitude <- xdf$longitude


# create map using plotly

fig <- life %>%
        plot_ly(
          type = 'densitymapbox',
          lat = ~latitude,
          lon = ~longitude,
          coloraxis = 'coloraxis',
          radius = 10)
fig <- fig %>%
  layout(
    mapbox = list(
      style ="open-street-map",
      zoom = 2.5,
      center = list(lon = -100, lat = 34)), 
    coloraxis = list(colorscale = "Viridis"))

fig  
