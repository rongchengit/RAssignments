library(mlbench)
library(randomForest)
library(ggplot2)
library(dplyr)
library(caret)
library(tibble)
library(partykit)
library(party)
library(gbm)
library(Cubist)
library(rpart)


set.seed(450)
X1 <- sample(1:1000 / 1000, 252, replace = TRUE)
X2 <- sample(1:100 / 100, 252, replace = TRUE)
X3 <- sample(1:10 / 10, 252, replace = TRUE)
X4 <- sample(1:5 / 5, 252, replace = TRUE)
X5 <- rep(1, 252)
Y <- X1 + rnorm(252, mean = 0, sd = 1.5) 

df <- data.frame(X1, X2, X3, X4, X5, Y)

rpart_model <- rpart(Y ~., data=df)

rpart_importance <- varImp(rpart_model)
print(rpart_importance)
#X1 and X2 are the most important predictors with .8
#X3 and X4 are the not as important
#x5 non existent
#contribution to the model

#8.3

