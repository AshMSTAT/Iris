library(xgboost)
library(tidyverse)
library(shiny)
library(DT)
library(gridExtra)
library(ggplot2)

IRISClass <- xgb.load("IRIS.model")
load("IRISClassInfo.rda")


generatePreds <- function(SepalLength = 5.8, SepalWidth = 3, PetalLength = 4.35, PetalWidth = 1.3){
  
  

  testDF <- t(as.matrix(c(
      SepalLength, SepalWidth, PetalLength, PetalWidth)
    ))

  preds <- predict(IRISClass, testDF)

  data.frame(
    species = IRISClassInfo$var.levels
    ,preds
  ) %>%
  arrange(desc(preds))

}

# Empty plot
empty <- ggplot()+geom_point(aes(1,1), color="white") +
  theme(                              
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )


