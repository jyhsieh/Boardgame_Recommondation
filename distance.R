library(cluster)
library(factoextra)

# load data ----
bg_data<-read.csv("/Users/JerryHsieh/Dropbox/Project/bgg_db_2017_04.csv",stringsAsFactor=FALSE)

Uniq_mechanic = unique(unlist(strsplit(bg_data$mechanic, ", ")))
Uniq_category = unique(unlist(strsplit(bg_data$category, ", ")))
hist(bg_data$year[which(bg_data$year>1950)],label=TRUE)

# mechanic count ----
Each_type_count = sapply(Uniq_mechanic, function(x) length(grep(x, bg_data$mechanic)))
Top10type = names(sort(Each_type_count[1:10], decreasing=TRUE))

# popular by owned & by ratings ---- 
avg_owned_type = sapply(Uniq_mechanic, function(x) mean(bg_data$owned[grep(x, bg_data$mechanic)]))
Top10owned_type = names(sort(avg_owned_type, decreasing=TRUE)[1:10])

avg_rating_type = sapply(Uniq_mechanic, function(x) mean(bg_data$geek_rating[grep(x, bg_data$mechanic)]))
Top10rating_type = names(sort(avg_rating_type, decreasing=TRUE)[1:10])

# expand the mechanic and the category variable as dataframe boardgame----
boardgame = bg_data
mechanic_df = as.data.frame(sapply(Uniq_mechanic, function(x) grepl(x, boardgame$mechanic)*1))
category_df = as.data.frame(sapply(Uniq_category, function(x) grepl(x, boardgame$category)*1))
boardgame = cbind(boardgame, mechanic_df, category_df)

# logit function ----
logit = function(x){
  return((1/(1+exp(-x)))*2-1)
}

# K-means 
non_clust_var = names(boardgame) %in% c("rank","bgg_url","game_id","min_players","max_players","min_time","max_time"
                  ,"year","avg_rating","geek_rating","num_votes","image_url","mechanic","owned","category","designer"
                  ,"weight","avg_time","age")
bg_clust = boardgame[which(boardgame$year>1980),]
bg_clust = bg_clust[!non_clust_var]
# bg_clust$avg_time = logit(bg_clust$avg_time)
# bg_clust$age = logit(bg_clust$age)
my_clust = kmeans(bg_clust[,-1],100)
cluster = my_clust$cluster
bg_clust = cbind(bg_clust, cluster)
# bg_clust[which(bg_clust$cluster == 1),"names"]

# related function ----
related = function(x){
  no_cluster = bg_clust[which(bg_clust$names == x), "cluster"]
  return(bg_clust[which(bg_clust$cluster == no_cluster),"names"])
}
related("Agricola")

# distance function ----
similar = function(x,n){
  temp = bg_clust[which(bg_clust$names == x),]
  dist = apply(bg_clust[which(bg_clust$cluster == temp$cluster),2:137], 1, 
               function(xx) sum((temp[2:137] - xx)^2))
  return(bg_clust[head(names(sort(dist)[-1]),n),1])
}
similar("Agricola",10)




