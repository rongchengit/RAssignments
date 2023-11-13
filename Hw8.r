#install.packages("e1071")
library(mlbench)
library(caret)
library(ggplot2)
library(lattice)
library(earth)
library(e1071)

set.seed(200)
trainingData <- mlbench.friedman1(200, sd = 1)
trainingData$x <- data.frame(trainingData$x)
#print(featurePlot(trainingData$x, trainingData$y))

testData <- mlbench.friedman1(5000, sd = 1)
testData$x <- data.frame(testData$x)

knnModel <- train(x = trainingData$x,
                  y = trainingData$y,
                  method = "knn",
                  preProc = c("center", "scale"),
                  tuneLength = 10)

# View model summary
print("knnModel")
print(knnModel)

knnPred <- predict(knnModel, newdata = testData$x)
print(postResample(pred = knnPred, obs = testData$y))

#-mars--------------------------------
marsFit <- earth(trainingData$x, trainingData$y)
marsFit

marsGrid <- expand.grid(.degree=1:2, .nprune=2:38)
marsModel <- train(trainingData$x, trainingData$y,
                   method="earth",
                   tuneGrid=marsGrid,
                   trControl=trainControl(method="cv"))

print("marsModel")
print(marsModel)

#-svm--------------------------------
marsPred <- predict(marsModel, newdata=testData$x)
print(postResample(pred=marsPred, obs=testData$y))

svmModel <- train(trainingData$x, trainingData$y,
                   method="svmRadial",
                   preProc=c("center", "scale"),
                   tuneLength=14,
                   trControl=trainControl(method="cv"))
print("svmModel")
print(svmModel)

svmPred <- predict(svmModel, newdata=testData$x)
print(postResample(pred=svmPred, obs=testData$y))

marsVarImp <- varImp(marsModel)
print("marsVarpImp")
print(marsVarImp)

#Mars performed the best
#lowest RMSE and MAE, and the highest R-squared value
#by 0.7796624 to svm RMSE
#by 0.5467170 to svm MAE
#by 0.1054316 to svm R-squared
#compared to knn it blew it away by a substantial margin
