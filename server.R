library(shiny)
library(tidyverse)
library(wordcloud2)
library(ggplot2)
#=========================================================================
# Expand Mechanic and Category Variable
#=========================================================================
All_mechanic = unlist(strsplit(boardgame$mechanic, ", "))
Uniq_mechanic = unique(All_mechanic)
All_cat = unlist(strsplit(boardgame$category, ", "))
Uniq_cat = unique(All_cat)

boardgame_cluster <- matrix(0,nrow = dim(boardgame)[1],ncol = length(Uniq_mechanic)+length(Uniq_cat))

for(i in 1:length(Uniq_mechanic)){
  boardgame_cluster[,i] <- grepl(Uniq_mechanic[i],boardgame$mechanic)*1
}
for(i in 1:length(Uniq_cat)){
  boardgame_cluster[,length(Uniq_mechanic)+i] <- grepl(Uniq_cat[i],boardgame$category)*1
}
dim(boardgame_cluster) # 4999 136
boardgame_cluster <- as.data.frame(boardgame_cluster)
colnames(boardgame_cluster)[1:length(Uniq_mechanic)] <- Uniq_mechanic
colnames(boardgame_cluster)[(1+length(Uniq_mechanic)):136] <- Uniq_cat
colnames(boardgame_cluster)[which(duplicated(colnames(boardgame_cluster)))] #"none"   "Memory"
which(colnames(boardgame_cluster)=="none") #50 114
colnames(boardgame_cluster)[50] <- "none_mechanic"
colnames(boardgame_cluster)[114] <- "none_cat"
which(colnames(boardgame_cluster)=="Memory") # 26 117
colnames(boardgame_cluster)[26] <- "Memory_mechanic"
colnames(boardgame_cluster)[117] <- "Memory_cat"
#========================================================
# cluster determined by max mode
#========================================================
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

cluster_max <- function(dat,n_cluster,num_iter){
  result <- matrix(0,nrow = dim(dat)[1],ncol = num_iter)
  for(i in 1:num_iter){
    result[,i] <- kmeans(dat,centers = n_cluster)$cluster
  }
  return(apply(result,MARGIN = 1,getmode))
}


#========================================================
server <- function(input,output){
  n_cluster <- reactive({input$num_cluster})
  i <- reactive({input$which_cluster})
  
  cluster_result <- reactive({
    cluster_max(boardgame_cluster,n_cluster = n_cluster(),num_iter = 20)
  })
  
  boardgame_cluster_all <- reactive({
    cbind(names = boardgame$names,boardgame_cluster,cluster = cluster_result())
  })
  
  d <- reactive({
    word_freq <- c()
    for(i in 1:n_cluster()){
      word_freq <- cbind(word_freq,apply(boardgame_cluster_all()[boardgame_cluster_all()$cluster==i,-c(1,138)],2,sum))
    }
    #=========================================================================
    # Word Cloud and Frequency Plot
    #=========================================================================
    data.frame(word=names(sort(word_freq[,i()],decreasing = T)),freq = sort(word_freq[,i()],decreasing = T))
  })
  output$wordcloud <- renderWordcloud2({
    set.seed(1234)
    if(d()$freq[1]>3*d()$freq[2]){
      size <- 0.1
    } else{size <- 0.5}
    wordcloud2(d(), size = size)
  })
  
  output$freqplot <- renderPlot({
    ggplot(data = d()[1:10,],aes(x = reorder(word,-freq),y = freq)) + 
      geom_bar(stat = "identity", fill = "steelblue") +
      xlab("Game Type")+ ylab("Frequency") +
      ggtitle(paste("Top 10 Game Types of Cluster ",i()))+
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size=24, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold")
      )
  })
  
  
  
  
}