# install.packages("nnet")
library(AppliedPredictiveModeling)
library(caret)
library(nnet)
library(ggplot2)
library(lattice)
library(earth)
library(e1071)

data("ChemicalManufacturingProcess")
nzv <- nearZeroVar(ChemicalManufacturingProcess)
ChemicalManufacturingProcess <- ChemicalManufacturingProcess[, -nzv]

preProcValues <- preProcess(ChemicalManufacturingProcess, method = c("knnImpute"))
data_imp <- predict(preProcValues, ChemicalManufacturingProcess)

set.seed(123)
index <- createDataPartition(data_imp$Yield, p=0.8, list=FALSE) 
Train <- data_imp[index, ]
Test <- data_imp[-index, ]

trans_train <- preProcess(Train, method = c("center", "scale"))
trans_test <- preProcess(Test, method = c("center", "scale"))

Train_prep <- predict(trans_train, Train)
Test_prep <- predict(trans_test, Test)

trainX <- Train_prep[, -which(names(Train_prep) == "Yield")]
trainY <- Train_prep$Yield
testX <- Test_prep[, -which(names(Test_prep) == "Yield")]
testY <- Test_prep$Yield

#KNN Model
knnModel <- train(x = trainX, y = trainY, method = "knn", preProcess = c("center", "scale"), tuneLength = 10)
knnPred <- predict(knnModel, newdata = testX)

#Neural Network Model
tooHigh <- findCorrelation(cor(trainX), cutoff = .75)
trainXnnet <- trainX[, -tooHigh]
testXnnet <- testX[, -tooHigh]

nnetGrid <- expand.grid(size = c(1:10), decay = c(0, 0.01, 0.1), bag = FALSE)
ctrl <- trainControl(method = "cv")

nnetTune <- train(trainXnnet, trainY, method = "avNNet", tuneGrid = nnetGrid, trControl = ctrl, linout = TRUE, trace = FALSE, MaxNWts = 10 * (ncol(trainXnnet) + 1) + 10 + 1, maxit = 500)
nnetPred <- predict(nnetTune, newdata = testXnnet)

#MARS Model
marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:38)
marsTuned <- train(trainX, trainY, method = "earth", tuneGrid = marsGrid, trControl = ctrl)
marsPred <- predict(marsTuned, newdata = testX)

#SVM Model
svmRTuned <- train(trainX, trainY, method = "svmRadial", tuneLength = 15, trControl = ctrl)
svmRPred <- predict(svmRTuned, newdata = testX)

trainSet <- rbind(
  mars = postResample(pred = predict(marsTuned, newdata = testX), obs = testY),
  svm = postResample(pred = predict(svmRTuned, newdata = testX), obs = testY),
  net = postResample(pred = predict(nnetTune, newdata = testXnnet), obs = testY),
  knn = postResample(pred = predict(knnModel, newdata = testX), obs = testY)
)
print("TrainSet")
print(trainSet)

testSet <- rbind(
  mars = postResample(pred = marsPred, obs = testY),
  svm = postResample(pred = svmRPred, obs = testY),
  net = postResample(pred = nnetPred, obs = testY),
  knn = postResample(pred = knnPred, obs = testY)
)
print("TestSet")
print(testSet)

print("svmImp")
print(varImp(svmRTuned))
print("marsImp")
print(varImp(marsTuned))
print("nnetImp")
print(varImp(nnetTune))
print("knnImp")
print(varImp(knnModel))

#Top 10
vip <- varImp(svmRTuned)$importance
top10Vars <- head(rownames(vip)[order(-vip$Overall)], 10)
print(as.data.frame(top10Vars))

#Plot
plotX <- df[,top10Vars]
plotY <- df[,colYield]
colnames(plotX) <- gsub("(Process|Material)", "", colnames(plotX))
print(featurePlot(plotX, plotY))