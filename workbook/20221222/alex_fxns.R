## stolen from https://github.com/albobson/SpotRfy/blob/master/scripts/spotRfy_master.R

## 22-12-22 ##

## Script to parse through downloaded Spotify code ##

## Written by Alexander Robertson (alexrob@uw.edu) ##

## Required packages
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)

#### Scripts to work with locally downloaded data ####

## Import the spotify streaming data
## The filepath needs to be the path to the file where the downloaded 
## data is stored
import_spotify_streaming_data <- function(datafile) {
  
  stream_files  <- list.files(path=as.character(datafile),
                              recursive=T,
                              pattern='StreamingHistory*',
                              full.names=T)
  
  ## Create a null dataset to store the streaming data
  data <- NULL
  
  ## For loop to extract from however many files you have
  for(i in 1:length(stream_files)) {
    new_data <- jsonlite::fromJSON(stream_files[i])
    data <- rbind(data, new_data)
    data
  }
  return(data)
}

## Clean Spotify streaming data
## This will add the various minutes/seconds columns and date column
## Need to specify desired timezone (eg. "PST")

clean_spotify_streaming_data <- function(data, your_timezone, include_pod) {
  ## Time is in ms, changing to minutes or seconds
  data$min_played <- data$msPlayed/60/1000
  data$sec_played <- data$msPlayed/1000
  
  ## Begin Ann's code
  
  ## crudely exclude podcasts by subsetting observations <13 min
  if(include_pod == FALSE){
    data <- filter(data, min_played < 13)
  } 
  
  ## End Ann's code
  
  ## Spotify data is recorded in UTC, need to convert to a different time
  data$dif_endtime <- lubridate::ymd_hm(data$endTime, tz = "UTC")
  data$dif_endtime <- with_tz(data$dif_endtime, tzone = as.character(your_timezone))
  
  ## Adding columns to break apart the date and time
  ## Extracting just the hour-min characters
  data$hm_endtime <- substr(data$dif_endtime, 11, 16)
  data$hm_endtime <- lubridate::hm(data$hm_endtime)
  data$day_endtime <- substr(data$dif_endtime, 1, 10)
  data$day_endtime <- ymd(data$day_endtime)
  
  ## Time of day played
  data$time <- as.numeric(data$hm_endtime)
  data$hour <- data$time*24/86340
  
  data1 <- data %>%
    group_by(artistName) %>%
    dplyr::mutate(sum_min_per_artist = sum(min_played), numb_songs = n()) %>%
    dplyr::arrange(desc(sum_min_per_artist)) %>% 
    group_by(trackName, .add = TRUE) %>%
    dplyr::mutate(sum_min_per_song = sum(min_played), num_times_song_played = n()) %>%
    dplyr::arrange(desc(sum_min_per_artist)) %>%
    dplyr::ungroup()
  
  
  return(data1)
}

## A function to plot streams by time of day and date
## artist_cutoff - how many of your top artists to show

plot_streaming_timeofday <- function(clean_stream_data, artist_cutoff = 10) {
  func_df <- clean_stream_data
  
  # return(func_df)
  
  artist_names <- func_df %>%
    select(artistName, sum_min_per_artist) %>%
    distinct() %>%
    arrange(desc(sum_min_per_artist)) %>%
    top_n(n = artist_cutoff, wt = sum_min_per_artist)
  
  # return(artist_names)
  
  func_df$top_artists <- ifelse(func_df$artistName %in% artist_names$artistName,
                                func_df$artistName, paste0("Artists Under Top ", artist_cutoff))
  
  # return(func_df)
  
  plot1 <- ggplot2::ggplot(func_df, aes(x = day_endtime, y = hour, 
                                        color = top_artists, alpha = 0.8,
                                        text = paste0("Date: ", day_endtime, "\n",
                                                      "Hour of the day: ", round(hour,2), "\n",
                                                      "Artist name: ", artistName, "\n",
                                                      "Track name: ", trackName))) +
    geom_point() +
    labs(color = paste0("Top ", artist_cutoff, " Artist"), alpha = "") +
    xlab("Date") +
    scale_y_continuous(limits = c(0, 24), breaks = (0:12)*2) +
    theme_light()
  
  plotly1 <- plotly::ggplotly(plot1, tooltip = "text")
  
  return(plotly1)
  
}

## A function to plot a barplot of each artist's time listened stacked by songs
## artist_cutoff - must specify how many of your top artists to show
plot_streaming_artists <- function(clean_stream_data, artist_cutoff = 10) {
  
  artist_names <- clean_stream_data %>%
    select(artistName, sum_min_per_artist) %>%
    distinct() %>%
    arrange(desc(sum_min_per_artist)) %>%
    top_n(n = artist_cutoff, wt = sum_min_per_artist)
  
  # return(artist_names)
  
  clean_stream_data$artistName <- ifelse(clean_stream_data$artistName %in% artist_names$artistName,
                                         clean_stream_data$artistName, NA)
  
  clean_stream_data$trackName <- ifelse(clean_stream_data$artistName %in% artist_names$artistName,
                                        clean_stream_data$trackName, NA)
  
  clean_stream_data <- clean_stream_data %>%
    select(trackName, artistName, sum_min_per_artist, 
           sum_min_per_song, numb_songs, num_times_song_played) %>%
    na.omit() %>%
    distinct()
  
  plot2 <- ggplot2::ggplot(clean_stream_data, 
                           aes(x = reorder(artistName, -sum_min_per_artist), 
                               y = sum_min_per_song, fill = trackName,
                               text = paste0("Artist name: ", artistName, "\n",
                                             "Track name: ", trackName, "\n",
                                             "Minutes listened to song: ", sum_min_per_song))) +
    geom_bar(stat = 'identity', position = 'stack') +
    theme_light() +
    theme(axis.text.x = element_text(angle = 90), legend.position = 'none') +
    ggtitle(paste0("Top ", artist_cutoff, " Artists")) +
    ylab("Total time listened to artist (min)") +
    xlab("")
  
  plotly2 <- plotly::ggplotly(plot2, tooltip = "text")
  
  return(plotly2)
}