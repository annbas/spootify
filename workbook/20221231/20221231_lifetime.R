############################## Lifetime Data ###################################
# 
# Analyses in this script:
#   -Create a calendar heat map to see listening patterns throughout a week
# 
################################################################################

library(lubridate)
source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")

#### using Alex's functions to import lifetime data ####
# read in Alex's functions
source('workbook/20221222/alex_fxns.R')

life0 <- import_lifetime_streaming_data('data/ignored_data')

life <- get_lifetime_tracks(life0)

#### creating calendar heat map ####
# Alex's code from functions

## Time is in ms, changing to minutes or seconds
life$min_played <- life$msPlayed/60/1000
life$sec_played <- life$msPlayed/1000

## Spotify data is recorded in UTC, need to convert to a different time
life$dif_endtime <- lubridate::ymd_hms(life$datetime, tz = "UTC")
life$dif_endtime <- with_tz(life$dif_endtime, tzone = "EST")

## Adding columns to break apart the date and time
## Extracting just the hour-min characters
life$hm_endtime <- substr(life$dif_endtime, 11, 16)
life$hm_endtime <- lubridate::hm(life$hm_endtime)
life$day_endtime <- substr(life$dif_endtime, 1, 10)
life$day_endtime <- ymd(life$day_endtime)

## Time of day played
life$time <- as.numeric(life$hm_endtime)
life$hour <- life$time*24/86340


#try out calendar heatmap function
g2g <- c('#DBF9C0','#1DB954', '#0a5d00', '#063b00')

life.plot <- life %>%
              filter(day_endtime >= '2017-01-01') %>% #function cannot handle more than six years, so sub accordingly
              group_by(day_endtime) %>% 
              summarise(min_played = sum(min_played)) #sum up minutes played on each date

calendarHeat(life.plot$day_endtime, life.plot$min_played, #date and minutes played per date
             ncolors = 99, color = "g2g", 
             varname="Daily Listening Habits (minutes per day)")

