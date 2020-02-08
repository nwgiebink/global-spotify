# Spotify API 
# 2020-1-29
# Noah Giebink

# packages
devtools::install_github('charlie86/spotifyr')
library(spotifyr)
library(tidyverse)

# Spotify API credentials ----
client_id <- read.table('client_id')
client_sec <- read.table('client_secret')
Sys.setenv(SPOTIFY_CLIENT_ID = client_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = client_sec)
token <- get_spotify_access_token()

# Get top 50 from various countries ----
# get my playlists
my_pl <- get_my_playlists(limit = 23) # subbed 23 countries top 50
unique(my_pl$name) # make sure all countries are there

# get tracks for each playlist
# tracks <- get_playlist_tracks(my_pl$id, limit = 1500)
# I think it can only do one at a time because
# it works with just one at a time.

# populate list with tracklist df's one at a time...
tracks_list <- list()
for(i in 1:length(my_pl$id)){
  tracks_list[[i]] <- get_playlist_tracks(my_pl[i,]$id)
} 

# will need to make a column in each df with playlist name
# can extract country name from playlist name later...
tracks_list2 <- tracks_list

name <- my_pl$name
for(i in 1:length(tracks_list)){
 tracks_list2[[i]] <- mutate(tracks_list[[i]], name = name[i])
}

# get track audio features ----
# returns audio features (18 variables) for every song
# limit = features for 100 songs

# initialize list of audio features data frames
audio_feat <- list()
for(i in 1:length(tracks_list2)){
  audio_feat[[i]] <- get_track_audio_features(tracks_list2[[i]]$track.id)
}

# left_join audio_feat[[i]] to track_list2[[i]]
tracks_list3 <- list()
for(i in 1:length(tracks_list2)){
  tracks_list3[[i]] <- cbind(tracks_list2[[i]], audio_feat[[i]])
}

# compile tracks list into a single dataframe ----
top_tracks <- data.frame() # initialize top_tracks data frame
for(i in 1:length(tracks_list)){
  top_tracks <- rbind(top_tracks, tracks_list3[[i]])
}
glimpse(top_tracks)

top_tracks <- readRDS('data/top_tracks.rds')


