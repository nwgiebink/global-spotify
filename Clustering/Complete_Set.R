
library(tidyverse)
library(factoextra)
library(Rtsne)
library(caret)
library(cluster)
library(dbscan)
install.packages("BBmisc")
library(BBmisc)
# read the data in
spot_in <- read_csv("/data/spot_clean.csv")


# delete the attributes which should not be in
spot <- spot_in %>% select(-track.id, -track.name, -track.track_number, -track.type, -track.album.album_type, 
                           -track.album.id,-track.album.name, -track.album.release_date, -track.album.total_tracks)


# change mode to minor and major (major == 1, minor == 2)
spot$mode <- if_else(spot$mode == 1, 1, 2)

# make chr and binary into factor

spot$country <- as.factor(spot$country)

#gower distance matrix
gower_dist_1 <- daisy(spot,
                    metric = "gower",
                    type = list(asymm = 1))

summary(gower_dist_1)
  
gower_mat_1 <- as.matrix(gower_dist_1)

# checking for number of clusters using silhouette coefficient

sil <- c(NA)

for(i in 2:50){
  pam_fit <- pam(gower_mat, diss = TRUE, k = i)
  
  sil[i] <- pam_fit$silinfo$avg.width
}

plot(1:50, sil)
lines(1:50, sil)

# found k = 23 as best (which is the number of countries in the set)

pam_fit_1 <- pam(gower_mat_1, k = 23, diss = TRUE)

# add the cluster to the original data
spot_cluster_1 <- data.frame(spot, pam_fit_1$clustering)

res_1 <- spot_cluster_1 %>% 
  mutate(cluster = pam_fit_1.clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

res_1$the_summary

# looking at the 23 medoids
spot_cluster_1[pam_fit_1$medoids,]

# trying to plot 

tsne_obj_1 <- Rtsne(gower_dist_1, is_distance = TRUE)

tsne_data_1 <- tsne_obj_1$Y %>%
  data.frame() %>%
  setNames(c("X","Y")) %>%
  mutate(cluster = factor(pam_fit_1$clustering),name = spot_cluster_1$country)

ggplot(aes(x=X,y=Y), data = tsne_data_1) + 
  geom_point(aes(color=cluster))





# does not seem to be very usefull, so new try: #############################################################################

spot_in <- read_csv("C:/Users/Sebastian/Dropbox/A_UofA/INFO 523/final_project/global-spotify/data/spot_clean.csv")
spot <- spot_in %>% select(-track.id, -track.name, -track.track_number, -track.type, -track.album.album_type, 
                           -track.album.id,-track.album.name, -track.album.release_date, -track.album.total_tracks)
spot$mode <- if_else(spot$mode == 1, 1, 2)

spot$country <- as.factor(spot$country)


#gower distance matrix without country
gower_dist <- daisy(spot[,-16],
                    metric = "gower",
                    type = list(asymm = 1))

summary(gower_dist)

gower_mat <- as.matrix(gower_dist)

# checking for number of clusters using silhouette coefficient

sil <- c(NA)

for(i in 2:25){
  pam_fit <- pam(gower_mat, diss = TRUE, k = i)
  
  sil[i] <- pam_fit$silinfo$avg.width
}

plot(1:25, sil)
lines(1:25, sil)

# found k = 14 as best 

pam_fit <- pam(gower_mat, k = 14, diss = TRUE)

# add the cluster to the original data
spot_cluster <- data.frame(spot, pam_fit$clustering)

res2 <- spot_cluster %>% 
  mutate(cluster = pam_fit.clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

res2$the_summary

# looking at the 14 medoids
spot_cluster[pam_fit$medoids,]

# trying to plot 

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X","Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),name = spot_cluster$country)

ggplot(aes(x=X,y=Y), data = tsne_data) + 
  geom_point(aes(color=cluster))


# and again, trying an DBSCAN cluster #####################################################################################

spot_dbscan_in <- spot_in %>% select(-track.id, -track.name, -track.track_number, -track.type, -track.album.album_type, 
                                     -track.album.id,-track.album.name, -track.album.release_date, -track.album.total_tracks)
spot_dbscan <- spot_optics_in %>% select(-track.explicit, -country)

# setting minPts = 19 for dbscan and a bit higher for optics, determining eps by looking at the knn-knee
kNNdistplot(spot_optics, k=19)
abline(h=38, col ="red", lty=2) 

dbscan_res <- dbscan(spot_dbscan, eps=38, minPts = 19)
dbscan_res$cluster
 
# and now? don't know. Maybe bound the cluster back to the data and gain some insight?????????????

spot_dbscan_cluster <- data.frame(spot_dbscan,dbscan_res$cluster,spot_dbscan_in$country)


# do the summary 
res_dbscan <- spot_dbscan_cluster %>% 
  mutate(cluster = dbscan_res.cluster) %>%
  mutate(country = spot_dbscan_in.country) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))


res_dbscan$the_summary





# and again, trying an OPTICS cluster #####################################################################################

# delete the attributes which should not be in
spot_optics_in <- spot_in %>% select(-track.id, -track.name, -track.track_number, -track.type, -track.album.album_type, 
                           -track.album.id,-track.album.name, -track.album.release_date, -track.album.total_tracks)

# deselect country and explicit mode
spot_optics <- spot_optics_in %>% select(-track.explicit, -country)

# don't know how to choose the right number of minpoints, looks different for 25 and 30
opt_res <- optics(spot_optics, eps= 38, minPts = 30)
opt_res

# check the order
head(opt_res$order, n=50)
tail(opt_res$order, n=50)

# plot the result
plot(opt_res)
abline(h=35, col ="red", lty=2) 

# try again using gower dist
#gower distance matrix without country
gower_dist_opt <- daisy(spot_optics[,-16],
                      metric = "manhatten",
                      type = list(asymm = 1))

summary(gower_dist_opt)

gower_mat <- as.matrix(gower_dist)

kNNdistplot(gower_dist_opt, k=19)
abline(h=0.14, col ="red", lty=2) 

# following chp10.doc
(res_col <- optics(gower_dist, eps=10, minPts = 10))
plot(res_col)
(res_col_d <- extractDBSCAN(res_col, eps_cl=0.14))



#################################################################################################################
spot_in <- read_csv("data/spot_clean.csv")
# delete the attributes which should not be in
spot <- spot_in %>% select(-track.id, -track.name, -track.track_number, -track.type, -track.album.album_type, 
                           -track.album.id,-track.album.name, -track.album.release_date, -track.album.total_tracks)

spot_social <- spot_in %>% select(gdp,freedom,density_sqkm,percent_internet_users,percent_urban,median_age,happiness,country)

spot_social <- unique(spot_social)
spot_social$country <- as.factor(spot_social$country)

#gower distance matrix without country
gower_dist <- daisy(spot_social,
                    metric = "gower")


summary(gower_dist)

gower_mat <- as.matrix(gower_dist)

# checking for number of clusters using silhouette coefficient

sil <- c(NA)

for(i in 2:22){
  pam_fit <- pam(gower_mat, diss = TRUE, k = i)
  
  sil[i] <- pam_fit$silinfo$avg.width
}

plot(1:22, sil)
lines(1:22, sil)

# found k = 2 as best 

pam_fit <- pam(gower_mat, k = 2, diss = TRUE)

# add the cluster to the original data
spot_cluster <- data.frame(spot_social, pam_fit$clustering)

spot_cluster1 <- spot_cluster %>% filter(pam_fit.clustering == 1) %>% select(country)
spot_cluster2 <- spot_cluster %>% filter(pam_fit.clustering == 2) %>% select(country)

spot_social_clusters <- mutate(spot_in, cluster = ifelse(country %in% spot_cluster1$country,1,2))



res2 <- spot_cluster %>% 
  rename(cluster = pam_fit.clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

res2$the_summary

spot$track.explicit <- ifelse(spot$track.explicit == FALSE, 0,1)
spot_scale <- spot %>% select(track.explicit, track.popularity,danceability,energy,loudness,mode, speechiness,acousticness, instrumentalness, liveness,valence, tempo, duration_min)

# write the own scale funtion

scale_self <- function(x){
  norm=(x-min(x))/(max(x) - min(x))
  return(norm)
}

spot_scale <- data.frame(apply(spot_scale, MARGIN=2, FUN=scale_self))

library(reshape2)
ggplot(melt(spot_scale), aes(variable, value, color = variable, fill = variable)) + 
  geom_boxplot() + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
 
