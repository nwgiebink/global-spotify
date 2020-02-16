# Preprocessing
# 2020/2/16
# Noah Giebink

# packages
library(tidyverse)
library(lubridate)

# data
top_tracks <- readRDS('data/top_tracks.rds')

# select relevant columns
top_tracks_clean <- select(top_tracks, -added_at, -is_local, -primary_color, -contains('added'), -contains('.uri'),
                           -contains('href'), -contains('url'), -track.artists, -track.available_markets, 
                           -track.disc_number, -track.episode, -track.is_local, -track.track,
                           -track.album.artists, -track.album.available_markets, -track.album.images, 
                           -track.album.release_date_precision, -track.album.type, -track.external_ids.isrc,
                           -type, -id, -duration_ms)


