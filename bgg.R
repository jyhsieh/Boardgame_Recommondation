library(cluster)
library(tidyverse)
library(ggplot2)
library(factoextra)

# load data ----
bg_data<-read.csv("/Users/JerryHsieh/Dropbox/Project/bgg_db_2017_04.csv",stringsAsFactor=FALSE)

Uniq_mechanic = unique(unlist(strsplit(bg_data$mechanic, ", ")))
Uniq_category = unique(unlist(strsplit(bg_data$category, ", ")))

boardgame = bg_data
mechanic_df = as.data.frame(sapply(Uniq_mechanic, function(x) grepl(x, boardgame$mechanic)*1))
category_df = as.data.frame(sapply(Uniq_category, function(x) grepl(x, boardgame$category)*1))
boardgame = cbind(boardgame, mechanic_df, category_df)

boardgame = boardgame[!duplicated(boardgame$names),]
# boardgame = boardgame[which(boardgame$year>2000),]
# boardgame = boardgame[which(boardgame$owned>500),]
row.names(boardgame) = boardgame$names

var = names(boardgame) %in% c("rank","bgg_url","game_id","min_players","max_players","min_time","max_time"
                               ,"year","avg_rating","geek_rating","num_votes","image_url","mechanic","owned","category","designer"
                               ,"weight","avg_time","age","names")
boardgame= boardgame[!var]

boardgame <- na.omit(boardgame)
boardgame <- scale(boardgame)

# distance <- get_dist(boardgame)

k2 <- kmeans(boardgame, centers = 7, nstart = 25)
# fviz_cluster(k2, data = boardgame)

fviz_nbclust(boardgame, kmeans, method = "wss") # bend the knee 7
fviz_nbclust(boardgame, kmeans, method = "silhouette") # 2 (max) or 6 (2nd)

cluster = my_clust$cluster
bg_clust = cbind(bg_clust, cluster)

