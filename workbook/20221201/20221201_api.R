################### Playing around with Spotify API ############################
#
# Analyses in this script:
#   -most common genres associated with top artists (weird)
#   -analyzing my oldest playlist
#     -isolating interesting vatiables (like danceability, valence, etc.),
#      calculating mean of variables, creating correlation network (also weird), 
#      creating a heatmap
#    
################################################################################

#### LIBRARIES ####

#install.packages('spotifyr', dependencies = T)
library(spotifyr)
library(corrr)
library(shadowtext)
library(jsonlite)
library(lubridate)
library(gghighlight)
library(tidyverse)
library(plotly)
library(heatmaply)

source('spotifyr/workbook/authentication.R')

#### AUTHENTICATION ####

# in .gitignore file, but code is
# 
# Sys.setenv(SPOTIFY_CLIENT_ID = 'XXXXXXXXXXXXXXXXXXXXXXXXX')
# Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXXXXXXXXXXXXXXXXXXXXXXXX')
# 
# access_token <- get_spotify_access_token()


#### GENRES ASSOCIATED WITH TOP ARTISTS ####

#get top artists
top_artists <- get_my_top_artists_or_tracks(type = 'artists', 
                                            time_range = 'long_term', 
                                            limit = 50, 
                                            authorization = get_spotify_authorization_code()
) %>% 
  select(name, genres) %>% 
  rowwise %>% 
  mutate(genres = paste(genres, collapse = ', ')) %>% 
  ungroup %>%
  kable()

#only extract top genres from my top artists

top_genres <- get_my_top_artists_or_tracks(type = 'artists', 
                                           time_range = 'long_term', 
                                           limit = 50, 
                                           authorization = get_spotify_authorization_code()
) %>% 
  select(genres) %>%
  unlist() 

top_genres <- as.data.frame(table(top_genres)) #get counts

#only include genres that appear more than once in dataset
top_genres <- top_genres %>%
  filter(Freq > 1) 

#order by decreasing frequency
top_genres <- top_genres[order(top_genres$Freq, decreasing = T), ]

#generate plot
genre_plt <- top_genres %>%
  mutate(top_genres = fct_reorder(top_genres, Freq)) %>% #reordering top genres by frequency
  ggplot() +
  geom_col(aes(Freq, top_genres), fill = "#1DB954", width = 0.6) + 
  scale_x_continuous(
    limits = c(0, 17),
    breaks = seq(0, 17, by = 5), 
    expand = c(0, 0), # The horizontal axis does not extend to either side
    position = "top"  # Labels are located on the top
  ) +
  # The vertical axis only extends upwards 
  scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines for the horizontal axis
    panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Remove the title for both axes
    axis.title = element_blank(),
    # Only left line of the vertical axis is painted in black
    axis.line.y.left = element_line(color = "black"),
    # But customize labels for the horizontal axis
    axis.text.x = element_text(#family = "Econ Sans Cnd", 
      size = 10)
  ) +
  ggtitle('Frequency of Genres Associated with my Top 50 Spotify Artists')

#export plot
ggsave(filename = "genre_freq.png",width = 7, height = 8, units = "in", device='png', dpi=300)

#### ANALYZING TUNE VOID ####

#read in my playlists
user_id <- 'victorialokelani'
playlists <- get_user_playlists(user_id,
                                limit = 20,
                                offset = 0,
                                authorization = get_spotify_authorization_code())
#pull id from tune void playlist
playlist_uris <- playlists %>%
  filter(name == "tune void (est. 2014)") %>%
  select(id) %>%
  pull(id)

#pull in audio features of tune void
tune_void <- get_playlist_audio_features(user_id, playlist_uris)

#keep these columns
tune_void_columns_to_keep <- c("danceability", "energy", "key", "loudness", 
                               "mode", "speechiness", "acousticness", 
                               "instrumentalness", "liveness", "valence", "tempo", 
                               "time_signature", "track.artists", 
                               "track.duration_ms", "track.name", "track.popularity", 
                               "track.album.name", "track.album.release_date", 
                               "key_name", "mode_name", "key_mode")

#create playlist with these columns
tune_void2 <- tune_void %>%
  select(all_of(tune_void_columns_to_keep))

#create a correlation network
cor <- correlate(tune_void2[,c(1:2, 4, 6:11)])
network_plot(cor, min_cor = 0.01)

#find average of num values
tune_void2 %>% 
  select(c(1:2, 4, 6:11)) %>%
  colMeans()

# turn song names into row names (only 50 most recent songs added)
tune_void_norm <- as.data.frame(tune_void2)
songs <- tail(tune_void_norm[,15], n = 50)
tune_void_norm <- tail(scale(tune_void_norm[,c(1:2,4,6:11)]), n = 50)
rownames(tune_void_norm) <- songs

# subset last 100 rows of tune void and normalize data
tune_void_norm <- as.matrix(tune_void_norm)


# trying to build a heatmap of 50 most recent songs added to tune void
hmap <- heatmaply(
          tune_void_norm,
          k_col = 2, 
          k_row = 2
        )
