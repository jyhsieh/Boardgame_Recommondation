Board Game
================
Jui-Ying Hsieh, Li-Hsin Chen

### 1. Background

We enjoy playing baord games. We have collected more than 20 types of board games, and the number is still growing. We like to explore games, but play one new games after another blindly is not smart. There are evidence shown that more and more games are being created; 1600 games released in the last five years, while only 1086 games released in the 19th century. To explore games efficiently, we frequently visit the famous board game website BoardGameGeek (BGG). There is a comprehensive list of games including their rankings. Our approach to explore games is to review their rankings. We did actually find enjoyable games such as Agricola, Peurto Rico and Pandemic.

However, as we collect more and more games, we find out that rankings does not making us to find enjoyable games. It is obvious that most of the top ranking games are strategy games. Other types of games typically are not able to go to top ranking because BGG's users prefer "heavier" games. For example, one of our favorite board games "cat in the sack" is ranked 1077 in BGG, but we really enjoy playing this "light" game. So we think this game, and very likely other "light" games, are way underestimated.

Hence, we grabbed the data from BGG, and tried to build a "fair" board game reference system that help players find their favorite games. This project is about designing a board game reference system, Board Game Match, that finds the most enjoyable games for users. We hope Board Game Match can save players time and effort to explore new games.

### 2. Method

Several fantastic algorithms have been invented and used for recommendation systems in the past few years. The choice of algorithms highly depends on the data used for the system. Since the data we have contains only information of boardgame, we choose [item-based collaborative filtering](https://en.wikipedia.org/wiki/Item-item_collaborative_filtering) method. In this method, recommendation is made by the similarity of items among each others, that is, how similar they are according to their features.

This project is done in R with the following packages:

``` r
library(tidyverse)
library(ggplot2)
library(wordcloud2)
library(wordcloud)
boardgame <- read_csv("~/bgg_db_2018_01.csv")
```

Dataset is obtained from [Kaggle](https://www.kaggle.com/mrpantherson/board-game-data#bgg_db_2018_01.csv). This dataset contains many interesting features of about 5,000 boardgames. These features include:

``` r
colnames(boardgame)
```

    ##  [1] "rank"        "bgg_url"     "game_id"     "names"       "min_players"
    ##  [6] "max_players" "avg_time"    "min_time"    "max_time"    "year"       
    ## [11] "avg_rating"  "geek_rating" "num_votes"   "image_url"   "age"        
    ## [16] "mechanic"    "owned"       "category"    "designer"    "weight"

In this section, we will go through the process of building a recommendation system for boardgames. Data preprocessing will be conducted in sector 2.1. To reduce the calculation of pair-wise similarities, clustering will be applied on the processed data in section 2.2. Similarity between boardgames defined in sector 2.3 will be calculated only within the same cluster. Finally, we can make recommendations for boardgames in sector 2.4 based on the similarity we defined.

#### 2.1 Data Processing

We delete the duplicated board games and remove the missing values.

Instead of considering all features of boardgames, we use only "mechanic" and "category" in our system. There are 52 different mechanic types and 84 different categories. Hence, 136 (52 + 84) dummy variables are created to represent mechanic and category variables.

#### 2.2 Clustering

Since there are about 5,000 boardgames in the dataset, it will be computationally expensive to calculate simliarities for all possible pair-wise combinations. Hence, we first cluster the boardgames into few groups, and then calculated similarities for boardgames within the same group.

[K-means](https://en.wikipedia.org/wiki/K-means_clustering) is one of the most popular and intuitive clustering method. This method aims at finding k clusters such that the within-cluster sum of squares is minimized. The result of k-means is affected by the initialization of k points. Hence, k-means is performed several times (default 100 times) and the cluster with max count is assigned to each boardgames. Here, we use *k* = 5 as an example:

#### 2.3 Similarity

#### 2.4 Recommendation System



### 3. Result


### 4. Future Work

\*\* Combine current data with customer's data \*\* make better prediction
