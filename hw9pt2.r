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


library(AppliedPredictiveModeling)
data(ChemicalManufacturingProcess)

cmp_impute <- preProcess(ChemicalManufacturingProcess[,-c(1)], method=c('bagImpute'))

cmp <- predict(cmp_impute, ChemicalManufacturingProcess[,-c(1)])
set.seed(480)
train_r <- createDataPartition(ChemicalManufacturingProcess$Yield, p=0.8, list=FALSE)
X_train <- cmp[train_r,]
y_train <- ChemicalManufacturingProcess$Yield[train_r]
X_test <- cmp[-train_r,]
y_test <- ChemicalManufacturingProcess$Yield[-train_r]

set.seed(330)
model_rpart <- train(x= X_train, y= y_train, method="rpart", tuneLength=10, control= rpart.control(maxdepth=2))
print("model_rpart")
print(model_rpart)

set.seed(340)
model_rf3<- train(X_train, y_train, method='rf', tuneLength = 10)
print(model_rf3)

set.seed(350)
grid <- expand.grid(n.trees=c(50, 100, 150, 200), 
                    interaction.depth=c(1, 5, 10, 15), 
                    shrinkage=c(0.01, 0.1, 0.5), 
                    n.minobsinnode=c(5, 10, 15))
model_gbm1 <- train(x = X_train,y = y_train, method = 'gbm',tuneGrid = grid, verbose = FALSE)

print(model_gbm1$bestTune)

#a
print(summary(resamples(list(Single_True = model_rpart, Random_Forest = model_rf3, Gradient_Boosting=model_gbm1))))

test_performance <- function(models, testData, testTarget) {
  method <- c()
  res <- data.frame()
  for(model in models){
    method <- c(method, model$method)
    pred <- predict(model, newdata=testData)
    res <- rbind(res, t(postResample(pred=pred, obs=testTarget)))
  }
  row.names(res) <- method
  return(res)
}
models <- list(model_rpart, model_rf3, model_gbm1)
performance <- test_performance(models, X_test, y_test)
print("performance")
print(performance)

#b
model_pls <- train(x = X_train,y = y_train, method='pls', metric='RMSE',
                   tuneLength=20, trControl = trainControl(method='cv'))
print("pls imp")
print((pls_imp = varImp(model_pls)))

set.seed(424)
svm_model <- train(x = X_train,y = y_train,
                        method = "svmRadial",
                        tuneLength=10,
                        preProc = c("center", "scale"))
print("svm imp")
print((svm_imp = varImp(svm_model)))

p1<-plot(svm_imp, top=10, main='SVM')
p2<-plot(pls_imp, top=10, main='PLS')
gbm_imp<-varImp(model_gbm1)
p3<-plot(gbm_imp, top=10, main='GBM')
print(gridExtra::grid.arrange(p1, p2,p3,  ncol = 3))

#c
print("final rpart")
print(model_rpart$finalModel)
print(rpart.plot::rpart.plot(model_rpart$finalModel, box.palette = "RdBu", shadow.col = "blue", nn = TRUE))