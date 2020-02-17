# Preprocessing
# 2020/2/16
# Noah Giebink

# packages
library(tidyverse)
library(lubridate)

# data
spot <- readRDS('data/top_tracks.rds')

# select relevant columns
spot_clean <- select(top_tracks, -added_at, -is_local, -primary_color, -contains('added'), -contains('uri'),
                           -contains('href'), -contains('url'), -track.artists, -track.available_markets, 
                           -track.disc_number, -track.episode, -track.is_local, -track.track,
                           -track.album.artists, -track.album.available_markets, -track.album.images, 
                           -track.album.release_date_precision, -track.album.type, -track.external_ids.isrc,
                           -type, -id, -duration_ms) %>% 
  mutate(duration_min = (track.duration_ms/1000)/60) %>%
  select(-track.duration_ms)

spot_clean <- mutate(spot_clean, country = str_remove(spot_clean$name, 
                                                      regex(' top 50', 
                                                            ignore_case = TRUE))) 
spot_clean <- mutate(spot_clean, country = str_replace(spot_clean$country, 
                                                       pattern = "China Hits 2019 - China - China 2019", 
                                                       replacement = 'China'))

# TEST CASE (not for assignment): did country labels work?

# rank countries by mean track popularity 
pop <- spot_clean %>% group_by(country) %>% 
  summarise(popularity = mean(track.popularity)) %>% 
  arrange(desc(popularity))

# 1) What attributes are in our data set?
glimpse(spot_clean)

#' 2. Do you have highly correlated attributes? 
#' How did you find out about the correlations or lack of correlations?

#' 3. Do you have numerical attributes that you might want to discretize? 
#' Try at least two methods and compare the differences.

#' 4. If you have categorical attributes, use the concept hierarchy generation heuristics 
#' (based on attribute value counts) suggested in the textbook to produce some concept hierarchies. 
#' How well is this approach work for your attributes?