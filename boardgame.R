library(tidyverse)
library(ggplot2)
library(Rtsne)
#=========================================================================
# Read Data
#=========================================================================
boardgame <- read_csv("C:\\Users\\nishi\\OneDrive\\Kaggle\\BoardGame\\bgg_db_2018_01.csv")
head(boardgame)
colnames(boardgame)
dim(boardgame)

#=========================================================================
# EDA
#=========================================================================
sort(boardgame$year)
boardgame[1083,] %>% View()
a = boardgame %>% filter(year>1980) %>% group_by(year) %>% summarise(n=n(), own=sum(owned))
boardgame %>% filter(year>1980) %>% group_by(year) %>% summarise(n=n()) %>% ggplot(aes(year,n)) + geom_area()
boardgame %>% filter(year>1980) %>% group_by(year) %>% summarise(own=sum(owned)) %>% ggplot(aes(year,own)) + geom_area()
plot(a$year,a$own,type = "l")
boardgame
boardgame %>% ggplot(aes(num_votes,owned)) + geom_point(color=boardgame$age+1)

boxplot(boardgame$age)
boxplot(boardgame$weight)
boardgame %>% ggplot(aes(weight)) + geom_density()
summary(boardgame$weight)
boardgame[which.min(boardgame$weight),4]
#=========================================================================
#=========================================================================




#=========================================================================
# Expand Mechanic and Category Variable
#=========================================================================
boardgame_cluster <- boardgame

All_mechanic = unlist(strsplit(boardgame$mechanic, ", "))
Uniq_mechanic = unique(All_mechanic)
All_cat = unlist(strsplit(boardgame$category, ", "))
Uniq_cat = unique(All_cat)

for(i in 1:length(Uniq_mechanic)){
  boardgame_cluster[,20+i] <- grepl(Uniq_mechanic[i],boardgame$mechanic)*1
}
for(i in 1:length(Uniq_cat)){
  boardgame_cluster[,72+i] <- grepl(Uniq_cat[i],boardgame$category)*1
}
dim(boardgame_cluster) # 4999 156
colnames(boardgame_cluster)[21:72] <- Uniq_mechanic
colnames(boardgame_cluster)[73:156] <- Uniq_cat
colnames(boardgame_cluster)
boardgame_cluster <- boardgame_cluster[,-c(1:4,11:14,16:19)]
dim(boardgame_cluster) # 4999 144
#boardgame_cluster %>% View()
colnames(boardgame_cluster)[1:20]
boardgame_cluster <- unique(boardgame_cluster)
#=========================================================================
# Clustering (K-means)
#=========================================================================
n_cluster <- 5

set.seed(123)
kmean_result <- kmeans(scale(boardgame_cluster),centers = n_cluster)
#boardgame_cluster['cluster'] <- kmean_result$cluster

# Visualize through t-sne
set.seed(123)
tsne_out <- Rtsne(as.matrix(scale(boardgame_cluster)),perplexity = 200)
plot(tsne_out$Y, col = kmean_result$cluster)
legend(20,20,1:n_cluster,col = 1:n_cluster,pch = 1,pt.lwd = 4)

# Visualize through PCA
library(MASS)
library(FactoMineR)
bgg_pca <- PCA(scale(boardgame_cluster),ncp = 6,graph = FALSE)
#plot(bgg_pca,choix = "var")
#plot(bgg_pca$ind$coord)
#legend(20,20,1:n_cluster,col = 1:n_cluster,pch = 1,pt.lwd = 4)

plot(bgg_pca,axes = c(1,2),habillage = "ind",col.hab = kmean_result$cluster,label = "none")
legend(20,10,1:n_cluster,col = 1:n_cluster,pch = 1,pt.lwd = 4)




























Each_type_count = sapply(Uniq_mechanic, function(x) length(grep(x, boardgame$mechanic)))
Top10type = names(sort(Each_type_count, decreasing=TRUE)[1:10])

# ---- popular by owned
avg_owned_type = sapply(Uniq_mechanic, function(x) mean(boardgame$owned[grep(x, boardgame$mechanic)]))
Top10owned_type = names(sort(avg_owned_type, decreasing=TRUE)[1:10])

# ---- popular by ratings
avg_rating_type = sapply(Uniq_mechanic, function(x) mean(boardgame$geek_rating[grep(x, boardgame$mechanic)]))
Top10rating_type = names(sort(avg_rating_type, decreasing=TRUE)[1:10])