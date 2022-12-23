###################### Stealing MORE of Alex's Code ############################
# 
# Analyses in this script:
#   -Use Alex's functions to prep streaming data and visualize it
# 
################################################################################

# read in Alex's functions
source('workbook/20221222/alex_fxns.R')

# import streaming data and clean
myStreamingData <- import_spotify_streaming_data(datafile = "data")

myCleanData <- clean_spotify_streaming_data(data = myStreamingData, 
                                            your_timezone = "EST",
                                            include_pod = FALSE)

# plot!!!!
plot_streaming_timeofday(myCleanData, artist_cutoff = 10)

plot_streaming_artists(myCleanData, artist_cutoff = 10)


