######################## Playing with Alex's Code ##############################
# 
# Analyses in this script:
#   -Looking at years since track release by rank
# 
################################################################################

#### LIBRARIES ####

library(tidyverse)
library(knitr)
library(lubridate)
library(spotifyr)
library(plotly)

source('spotifyr/workbook/authentication.R')


#### YEARS SINCE TRACK RELEASE BY RANK ####

top50lt <- get_my_top_artists_or_tracks(
  type = 'tracks', time_range = 'long_term', limit = 50
) %>%
  mutate(artist.name = map_chr(artists, function(x) x$name[1])) %>%
  mutate(duration_ms = as.numeric(duration_ms)) %>%
  mutate(duration_s = duration_ms/1000, duration_m = duration_ms/60000)

top50lt$rank <- 1:nrow(top50lt)

## Plotting the years since track release by rank
## Creating a variable that codes for how many days it's been since the track
## was released
top50lt$album.release_year <- substr(top50lt$album.release_date, 1, 4)
top50lt$album.release_year <- as.numeric(top50lt$album.release_year)
top50lt$years_since_release <- as.numeric(year(today()))-top50lt$album.release_year

## I have a song that was released this year, so I'm going to replace 0s in the 
## years_since_release with 0.5 and see if that shows up in plotly
top50lt$years_since_release <- ifelse(top50lt$years_since_release == 0, 
                                      0.5, top50lt$years_since_release)

ggplotly(
  ggplot(top50lt, aes(x = rank, y = years_since_release, fill = artist.name, color = name)) +
    geom_bar(stat = 'identity') +
    theme_light() +
    theme(legend.position = 'none') +
    ylab("years since track release") +
    ggtitle("Years Since Track Release by Rank")
)

ggplot(top50lt, aes(x = rank, y = years_since_release, fill = artist.name)) +
  geom_bar(stat = 'identity') +
  theme_light() +
  theme(legend.position = 'none') +
  ylab("years since track release") +
  ggtitle("Years Since Track Release by Rank")


# I'm just gonna tip this plot on its side and then order it by descending years since release
  ggplotly(
    ggplot(top50lt, aes(x = reorder(rank, +years_since_release), y = years_since_release, fill = artist.name, color = name)) +
      geom_bar(stat = 'identity') +
      theme_light() +
      theme(legend.position = 'none') +
      ylab("years since track release") +
      ggtitle("Years Since Track Release by Rank") +
      coord_flip() +
      theme(
      axis.text.y=element_blank(),  #remove y axis labels
      axis.ticks.y=element_blank()  #remove y axis ticks
      )
  )
