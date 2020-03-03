# Preprocessing
# 2020/2/16

# Noah Giebink, Sebastian Deimen


# packages
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("corrplot")
#install.packages('discretization')
#install.packages('ggpubr')
library(tidyverse)
library(lubridate)
library(corrplot)
library(discretization)
library(ggpubr)

# Data Cleaning ----
# data
spot <- readRDS('data/top_tracks.rds')


# clean spot -> spot_clean
# select relevant columns
spot_clean <- select(spot, -added_at, -is_local, -primary_color, -contains('added'), -contains('uri'),
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
spot_clean <- select(spot_clean, -name)
# finding and deleting missing values
spot_clean <- filter(spot_clean, complete.cases(spot_clean))


# TEST CASES (not for assignment): did country labels work?

# rank countries by mean track popularity 
pop <- spot_clean %>% group_by(country) %>% 
  summarise(popularity = mean(track.popularity)) %>% 
  arrange(desc(popularity))

# plot track popularity by country, ranked
ggplot(spot_clean, aes(reorder(country, -track.popularity), track.popularity)) +
  geom_violin(draw_quantiles = 0.5)+
  theme(axis.text.x = element_text(angle = 90))+
  xlab('Country') +
  ylab('Top 50 Track Popularity')

# 1. ---- 
#' What attributes are in our data set? 
glimpse(spot_clean)

#' We have 28 attributes, across data types logical, character, integer, 
#' double, and factor

# key should be a factor (discrete key categories)
spot_clean$key = factor(spot_clean$key)
# similarly, time signature should be a factor
spot_clean$time_signature = factor(spot_clean$time_signature)

# show attributes in data set again, with correct types
glimpse(spot_clean)

# 2. ----
#' Do you have highly correlated attributes? ----
#' How did you find out about the correlations or lack of correlations?
spot_num <- select_if(spot_clean, is.numeric)
spot_cor <- cor(spot_num) # make correlation matrix
corrplot(spot_cor) # find correlations by visualizing corrplot
# we can see that "loudness" is highly correlated with "energy", as well as "valence" is correlated with "danceability", "energy" and "loudness"

#' There are correlated variables that are mostly redundant, 
#' such as track.album.total_tracks and track.track_number
#' as well as some correlated variables that are likely components
#' derived from similar underlying dimensions, such as 
#' loudness and energy, and
#' valence, danceability, and loudness.

# 3. ----
#' Do you have numerical attributes that you might want to discretize? 
#' Try at least two methods and compare the differences.

# Variable to discretize: valence

# Overview: what is the distribution of valence values like?
qqnorm(spot_clean$valence)
hist(spot_clean$valence)
  # pretty normal, maybe a tad left-skewed

# is valence predicted by country?
val_lm <- lm(valence~country, data = spot_clean)       
# summary(val_lm) (p < 2.2e-16)

# Method 1: chi merge (package discretization)
# discretize attribute 'valence' over class 'country'
# note: maybe an odd example, but good for the exercise...
val_d <- select(spot_clean, valence, country)
val_d <- chiM(val_d)

# what are the discretized values?
unique(val_d$Disc.data$valence)

# add new column to spot_clean with discretized valence values
spot_clean$merged_valence <- val_d$Disc.data$valence

# compare with top-down, unsupervised method: histogram  
spot_clean$binned_valence <- cut(spot_clean$valence, 5, 
                                 labels=c(1,2,3,4,5))


# visually compare differences between chimerge and histogram methods
plot_merg <- ggplot(spot_clean, aes(x = merged_valence, y = valence))+
  geom_jitter(alpha = 0.2)
plot_bin <- ggplot(spot_clean, aes(x = binned_valence, y = valence))+
  geom_jitter(alpha = 0.2)
ggarrange(plot_merg, plot_bin) 



#' 3. Conclusion A: Without a solid class label to discretize over,
#' an arbitrary-but-even binning method like histogram might be better.
#' The chi merge bins are much more irregular in width and sample number. 
#' Chi merge made a HUGE bin close to the worldwide mean valence. 
#' This could be caused by difficulty in defining a significant bin
#' where many countries overlap in valence value,
#' which would lead to a larger and larger bin width. Perhaps?


# which discretization method preserves the countries' mean valence better?
# Normalize valence, binned_valence, and merged_valence to compare magnitude
norm <- function(variable){(variable - min(variable))/(max(variable) - min(variable))}

spot_ordered <- spot_clean %>% 
  select(country, valence, binned_valence, merged_valence) %>%
  group_by(country) %>%
  summarise(mean_valence = mean(valence), 
            mean_merged = mean(merged_valence),
            mean_binned = mean(as.numeric(binned_valence))) %>%
  mutate(mean_merged = norm(mean_merged), 
         mean_binned = norm(mean_binned),
         mean_valence = norm(mean_valence)) %>%
  arrange(desc(mean_valence))
spot_ordered # ranked list of countries ranked by mean valence for plots & regression

# plot mean normalized valence (by country) against mean normlized merged valence
ggplot(spot_ordered, aes(x = mean_merged, y = mean_valence))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0)
# plot mean normalized valence against mean binned valence
ggplot(spot_ordered, aes(x = mean_binned, y = mean_valence))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0)

r_merg <- summary(lm(spot_ordered$mean_valence~spot_ordered$mean_merged))
r_bin <- summary(lm(spot_ordered$mean_valence~spot_ordered$mean_binned))
print(paste('mean_valence ~ mean_merged adjusted R-squared =', r_merg$adj.r.squared))
print(paste('mean_valence ~ mean_binned adjusted R-squared =', r_bin$adj.r.squared))

#' 3. Conclusion B: the mean valence of a country is predicted SLIGHTLY better
#' by the mean of its valence discretized by histogram binning compared to chimerge

# 4. ----
#' If you have categorical attributes, use the concept hierarchy generation heuristics 
#' (based on attribute value counts) suggested in the textbook to produce some concept hierarchies. 
#' How well is this approach work for your attributes?

# list the nominal variables which may be included in the hierarchy
glimpse(select_if(spot_clean, is.character))

# select only the variables to be included in the hierarchy
hier <- select(spot_clean, song = track.id, album = track.album.id, playlist = country)

# Make tibbles with hierarchies ranked from lowest n (broadest) to highest n (most specific)

hier <- hier %>% pivot_longer(cols = c(song, album, playlist), 
                      names_to = 'layer', values_to = 'item') %>%
  distinct() %>%
  group_by(layer) %>%
  summarise(count = n()) %>%
  arrange(count)
hier
#' In the first case, I pre-selected three concepts that are actually related.
#' This resulted in a realistic hierarchy, where playlist is broadest, then
#' album, and finally song is most specific. 
hier2 <- select(spot_clean, track.id, track.name, 
                track.type, track.album.album_type, track.album.id,
                track.album.name, country)
hier2 <- hier2 %>% pivot_longer(cols = c(track.id, track.name, 
                                track.type, track.album.album_type, 
                                track.album.id, track.album.name, 
                                country),
                                names_to = 'layer', 
                                values_to = 'item') %>%
  distinct() %>%
  group_by(layer) %>%
  summarise(count = n()) %>%
  arrange(count)
hier2

#' in the second case, I selected all character variables
#' without caring whether they might be conceptually linked at all. 
#' The concept hierarchy based on observation count alone 
#' fails to make logical sense in this case because 
#' not all categorical variables are connected in the first place 
#' and some are equal in observations.


# NOT REQUIRED ----
# decision tree - classify binned_valence 
val_tree <- rpart(formula = binned_valence ~ track.popularity + 
                    danceability +
                    energy +
                    loudness +
                    speechiness +
                    acousticness +
                    instrumentalness +
                    liveness +
                    tempo, 
                  data = spot_clean,
                  method = 'class')

plot(val_tree, uniform=TRUE)
text(val_tree, use.n=TRUE, all=TRUE, cex=.8)
