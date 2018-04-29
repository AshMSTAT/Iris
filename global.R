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




# scatterplot of sepal length vs width
sepalscatter <- ggplot(data=iris,aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + 
  geom_point(aes(shape=Species), size=1.5) + 
  scale_color_manual(values = c("orange", "purple", "green")) + 
  theme(legend.position=c(.1,1),legend.justification=c(1,1)) +
  geom_smooth(method="loess") +
  annotate("point", x = mean(iris$Sepal.Length), y = mean(iris$Sepal.Width), size=4, colour = "red")


# marginal density of sepal length - plot on top
sepalplot_top <- ggplot(data=iris,aes(x=Sepal.Length, fill = Species)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("orange", "purple","green")) + 
  theme(legend.position = "none")+
  geom_vline(xintercept = mean(iris$Sepal.Length), col="red", lwd=1, lty=2)

# marginal density of sepal width- plot on the right
sepalplot_right <- ggplot(data=iris,aes(x=Sepal.Width, fill = Species)) + 
  geom_density(alpha=.5) + coord_flip() + 
  scale_fill_manual(values = c("orange", "purple","green")) + 
  theme(legend.position = "none") +
  geom_vline(xintercept = mean(iris$Sepal.Width), col="red", lwd=1, lty=2)

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





# scatterplot of petal length vs petal width
petalscatter <- ggplot(data=iris,aes(x=Petal.Length, y=Petal.Width, color=Species)) + 
  geom_point(aes(shape=Species), size=1.5) + 
  scale_color_manual(values = c("orange", "purple", "green")) + 
  theme(legend.position=c(.1,1),legend.justification=c(1,1)) +
  geom_smooth(method="loess") +
  annotate("point", x = mean(iris$Petal.Length), y = mean(iris$Petal.Width), size=4, colour = "red")


# marginal density of petal length - plot on top
petalplot_top <- ggplot(data=iris,aes(x=Petal.Length, fill = Species)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("orange", "purple","green")) + 
  theme(legend.position = "none") +
  geom_vline(xintercept = mean(iris$Petal.Length), col="red", lwd=1, lty=2)


# marginal density of petal width - plot on the right
petalplot_right <- ggplot(data=iris,aes(x=Petal.Width, fill = Species)) + 
  geom_density(alpha=.5) + coord_flip() + 
  scale_fill_manual(values = c("orange", "purple","green")) + 
  theme(legend.position = "none") +
  geom_vline(xintercept = mean(iris$Petal.Width), col="red", lwd=1, lty=2)


