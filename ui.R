library(shiny)
library(tidyverse)
library(wordcloud2)
library(ggplot2)
#========================================================
ui <- fluidPage(
  titlePanel("Boardgame Match",windowTitle = "Boardgame Match"),
  
  wellPanel(
    numericInput(inputId = "num_cluster",
                 label = "Enter number of clusters",
                 value = 3,min = 3,max = 10)
  ),
  wellPanel(
    numericInput(inputId = "which_cluster",
                 label = "Select which cluster",
                 value = 1,min = 1,max = 10)
  ),
  h3("Word Cloud"),
  wellPanel(
    wordcloud2Output(outputId = "wordcloud")
  ),
  h3("Frequency"),
  wellPanel(
    plotOutput(outputId = "freqplot")
  )
)