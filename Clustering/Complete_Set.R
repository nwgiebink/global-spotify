# Clustering R exercise
# Sebastian Deimen and Noah Giebink

#install.packages("BBmisc")
library(tidyverse)
library(factoextra)
library(Rtsne)
library(caret)
library(cluster)
library(dbscan)
library(BBmisc)
# read the data in
spot_in <- read_csv("data/spot_clean.csv")


# delete the attributes which should not be in
spot <- spot_in %>% select(-track.id, -track.name, -track.track_number, -track.type, -track.album.album_type, 
                           -track.album.id,-track.album.name, -track.album.release_date, -track.album.total_tracks)

# turning into a dataframe to avoid an error with daisy

spot <- as.data.frame(spot)


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

# sil <- c(NA)
# 
# for(i in 2:50){
#   pam_fit <- pam(gower_mat_1, diss = TRUE, k = i)
#   
#   sil[i] <- pam_fit$silinfo$avg.width
# }
# 
# saveRDS(sil, 'sil.rds')
sil <- readRDS('sil.rds')

# make nice plots
sil_to_plot <- data.frame(index = seq(1:50), sil = sil)

ggplot(sil_to_plot, aes(x=index,y = sil)) +     # "#069680","#960664","#380696")
  geom_point(color = "#069680", size =3) +
  geom_line(color = "#D35612") +
  geom_vline(xintercept = 23, linetype="dotted", color = "#380696", size=1) +
  geom_text(aes(23,0.08,label = "k = 23", vjust = 0, hjust = 1.5))+
  labs(x = "Number of clusters", y = "Silhouette Width") +
  ylim(0.08,0.33) +
  theme_classic() +
  theme(axis.title = element_text(size = rel(1.8)))



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

# deselect country and explicit mode

spot_dbscan_in <- spot_in %>% select(-track.id, -track.name, -track.track_number, -track.type, -track.album.album_type, 
                                     -track.album.id,-track.album.name, -track.album.release_date, -track.album.total_tracks)
spot_dbscan <- spot_dbscan_in %>% select(-track.explicit, -country)

# setting minPts = 19 for dbscan and a bit higher for optics, determining eps by looking at the knn-knee
kNNdistplot(spot_dbscan, k=19)
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

# using same eps from dbscan (taken from kneeplot)
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
spot_optics <- as.data.frame(spot_optics)
gower_dist_opt <- daisy(spot_optics, 
                        metric = "manhattan")

summary(gower_dist_opt)

gower_mat_opt <- as.matrix(gower_dist_opt)

kNNdistplot(gower_dist_opt, k=19)
abline(h=60, col ="red", lty=2) 

# following chp10.doc
# eps and minPts taken from KNNdisplot
(res_col <- optics(gower_dist_opt, eps=60, minPts = 19))
plot(res_col)
(res_col_d <- extractDBSCAN(res_col, eps_cl=0.14))

# Unfortunately, we were not able to recover any clusters using optics. 
# This is despite using eps and minPts values derived from KNNdistplot.
# It appears optics is highly sensitive to hyperparameter values, 
# which highlights the downside to this approach.


###########################################################################################################################


spot_in <- read_csv("data/spot_clean.csv")
# delete the attributes which should not be in
spot <- spot_in %>% select(-track.id, -track.name, -track.track_number, -track.type, -track.album.album_type, 
                           -track.album.id,-track.album.name, -track.album.release_date, -track.album.total_tracks)

# turn 'explicit' into a dbl
spot_in$track.explicit <- ifelse(spot_in$track.explicit == FALSE, 0,1)

# select the social components
spot_social <- spot_in %>% select(gdp,freedom,density_sqkm,percent_internet_users,percent_urban,median_age,happiness,country)

# use only the unique values from each country
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


sil_to_plot <- data.frame(index = seq(1:22), sil = sil)

ggplot(sil_to_plot, aes(x=index,y = sil)) +     # "#069680","#960664","#380696")
  geom_point(color = "#069680", size =3) +
  geom_line(color = "#D35612") +
  geom_vline(xintercept = 2, linetype="dotted", color = "#380696", size=1) +
  geom_text(aes(2,0,label = 2, hjust = -1 ))+
  labs(x = "Number of clusters", y = "Silhouette Width") +
  theme_classic() +
  theme(axis.title = element_text(size = rel(1.8)))



# found k = 2 as best 

pam_fit <- pam(gower_mat, k = 2, diss = TRUE)


# trying to plot 

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE, perplexity = 3)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X","Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),country = spot_social$country)

ggplot(aes(x=X,y=Y), data = tsne_data) + 
  geom_point(aes(color=cluster), size = 3)

# try to find the two red points far away from cluster one


tsne_data %>%
     filter(X > 30 & X < 40,
            Y > -20 & Y < 0) %>%
     left_join(spot_social, by = "country") %>%
     collect %>%
     .[["country"]]

# -> RUSSIA and CHINA

# add the cluster to the original data
spot_social_cluster <- data.frame(spot_social, pam_fit$clustering)  # spot_cluster is now spot_social_cluster

spot_cluster_one <- spot_social_cluster %>% filter(pam_fit.clustering == 1) %>% select(country)
spot_cluster_two <- spot_social_cluster %>% filter(pam_fit.clustering == 2) %>% select(country)

spot_complete <- mutate(spot_in, cluster = ifelse(country %in% spot_cluster_one$country,1,2))



res2 <- spot_social_cluster %>% 
  rename(cluster = pam_fit.clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

res2$the_summary


spot_scale <- spot_in %>% select(track.explicit, track.popularity,danceability,energy,loudness,mode, speechiness,acousticness, instrumentalness, liveness,valence, tempo, duration_min)

# write our own scale funtion

scale_self <- function(x){
  norm=(x-min(x))/(max(x) - min(x))
  return(norm)
}

spot_scale <- data.frame(apply(spot_scale, MARGIN=2, FUN=scale_self))

# adding the clusters back to spot_scale

spot_scale <- data.frame(spot_scale, spot_complete$cluster) %>%
  rename(cluster = spot_complete.cluster)

# Finally, make two data frames, one for each cluster and plot those

set_one <- spot_scale %>% filter(cluster == 1)
set_two <- spot_scale %>% filter(cluster == 2)

library(reshape2)

# plot cluster one
ggplot(melt(set_one[,-14]), aes(variable, value, color = variable, fill = variable)) + 
  geom_boxplot() + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),text = element_text(size=14)) + 
  theme(axis.title = element_text(size = rel(1.8))) +
  theme(legend.position = "none") +
  xlab('Cluster Red')

# plot cluster two 
  ggplot(melt(set_two[,-14]), aes(variable, value, color = variable, fill = variable)) + 
    geom_boxplot() + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),text = element_text(size=14)) + 
    theme(axis.title = element_text(size = rel(1.8))) +
    theme(legend.position = "none") +
    xlab('Cluster Blue')
