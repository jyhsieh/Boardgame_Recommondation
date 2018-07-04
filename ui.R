library(shiny)
library(tidyverse)
library(wordcloud2)
library(ggplot2)
#========================================================
ui <- fluidPage(
  titlePanel(h1("Boardgame Match",align = "center"),windowTitle = "Boardgame Match"),
  
  fluidRow(
    column(4,numericInput(inputId = "num_cluster",
                          label = "Enter number of clusters",
                          value = 3,min = 3,max = 10),offset = 1),
    column(4,numericInput(inputId = "which_cluster",
                          label = "Select which cluster",
                          value = 1,min = 1,max = 10),offset = 1)
  ),
  
  fluidRow(
    column(4,wordcloud2Output(outputId = "wordcloud"),offset = 1),
    column(4,plotOutput(outputId = "freqplot"),offset = 1)
  )
  
  
  
  
)