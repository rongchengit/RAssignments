#8.1

#install.packages("Cubist")
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

set.seed(200)
simulated <- mlbench.friedman1(200, sd = 1)
simulated <- cbind(simulated$x, simulated$y)
simulated <- as.data.frame(simulated)
colnames(simulated)[ncol(simulated)] <- "y"

#a
#V1, V2, and V4 are the most important predictors
model_rf <- randomForest(y ~ ., data = simulated, importance = TRUE, ntree = 1000)
rf_Imp <- varImp(model_rf, scale = FALSE)

print("rf_imp 1")
print(rf_Imp)

#b
#V1, V2, and V4 are again the most significant predictors
#it doesnt seem much of the predictors have change drastically
simulated$duplicate1 <- simulated$V1 + rnorm(200) * .1
print("correlation")
print(cor(simulated$duplicate1, simulated$V1))

model_rf2 <- randomForest(y ~ ., data= simulated, importance= TRUE, ntree= 1000)
rf_Imp2 <- varImp(model_rf2, scale=FALSE)

print("rf_Imp 2")
print(rf_Imp2)

#c
#V1, V2, and V4 are still the most impotant predictors
#however V4 which had high overall importance has lower conditional and unconditional importance
model_cforest <- cforest(y ~ ., data= simulated)
cf_imp3<-varimp(model_cforest) %>% sort(decreasing = TRUE)
cf_imp4<-varimp(model_cforest, conditional = TRUE) %>% sort(decreasing=TRUE)
combinedImp <- as.data.frame(cbind(Model2 = rf_Imp2, Conditional = cf_imp3, Unconditional = cf_imp4))
print("combined imp")
print(combinedImp)

#d
#V4 is the most important variable with 100
#followed by V1 and V2
gbmGrid = expand.grid(interaction.depth = seq(1,5, by=2), 
                      n.trees = seq(100, 1000, by = 100), 
                      shrinkage = 0.1, 
                      n.minobsinnode = 5)
model_gbm = train(y ~ ., data = simulated, 
                  tuneGrid = gbmGrid, verbose = FALSE, 
                  method = 'gbm')
gbm_imp<-varImp(model_gbm)

print("gbm_imp")
print(gbm_imp)

#with cubist it included v3 as well in predictors
model_cubist <- cubist(simulated[,-11], simulated[, 11])
cubist_imp<-varImp(model_cubist)

df = as.data.frame(cbind(gbm_imp$importance, cubist_imp))
names(df) = c("boosted", "cubist")
print(df)