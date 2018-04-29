library(xgboost)
library(tidyverse)

IRIS <- iris


y1 <- IRIS$Species
var.levels <- levels(y1)
y = as.integer(y1)-1


x = IRIS[,c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width')]
var.names <- names(x)
x = as.matrix(x)

params <- list(
  "objective" = "multi:softprob"
  ,"eval_metric" = "mlogloss"
  ,"num_class" = length(table(y))
  ,"eta" = .5
  ,"max_depth" = 3
  ,"nthread" = 8
)

cv.nround = 10

bst.cv <- xgb.cv(param = params, data = x, label = y
                 , nfold = 5, nrounds = cv.nround
                 , missing = NA, prediction = TRUE
                 )

nrounds = which.min(bst.cv$evaluation_log$test_mlogloss_mean)
bst.cv$evaluation_log[nrounds,]


IRISClass <- xgboost(param = params, data = x, label = y
                        ,nrounds = nrounds, missing = NA)


xgb.importance(var.names, model = IRISClass)

xgb.save(IRISClass, "IRIS.model")

IRISClassInfo <- list(
  var.names = var.names
  ,var.levels = var.levels
)

save(IRISClassInfo, file = 'IRISClassInfo.rda')


