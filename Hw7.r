#6.2 
library(caret)
library(dplyr)
library(RANN)
library(DataExplorer)
library(AppliedPredictiveModeling)

#a)loading data
data(permeability)

#b
print("pre sparsing fingerprint")
print(dim(fingerprints))
print(head(permeability))

sparseFeatures <- nearZeroVar(fingerprints)
nonSparseFeatures <- fingerprints[, -sparseFeatures]
print("spared fingerprint")
print(dim(nonSparseFeatures))

#c
dataFrame <- as.data.frame(fingerprints)
dataFrame <- dataFrame %>%
  mutate(target = permeability)

sparseFeatures <- nearZeroVar(dataFrame)
dataFrame <- dataFrame[, -sparseFeatures]
print(dim(dataFrame))


#train split
inTrain <- createDataPartition(dataFrame$target, times = 1, p = 0.8, list = FALSE)
trainingData <- dataFrame[inTrain, ]
testingData <- dataFrame[-inTrain, ]

#pls
plsModel <- train(target ~ ., data = trainingData, method = "pls",
  center = TRUE, trControl = trainControl("cv", number = 10),
  tuneLength = 25
)
print("trainedPLSModel")
print(plsModel)

#d
predictionsTestPLS <- predict(plsModel, testingData)
checkValuesPLS <- data.frame(obs = testingData$target, pred = predictionsTestPLS)
colnames(checkValuesPLS) <- c("obs","pred")
print("predict test plsModel")
print(defaultSummary(checkValuesPLS))
#ncomp = 6 is 0.6190885


#e
#ridge regression 
ridgeModel <- train(target ~ ., data = trainingData, method = "glmnet",
  center = TRUE, trControl = trainControl("cv", number = 10),
  tuneLength = 25, 
  tuneGrid = expand.grid(alpha = 0, lambda = seq(0.001, 0.1, length.out = 25))
)
print("trainedRidgeModel")
print(ridgeModel)

predictionsTestRidge <- predict(ridgeModel, testingData)
checkValuesRidge <- data.frame(obs = testingData$target, pred = predictionsTestRidge)
colnames(checkValuesRidge) <- c("obs","pred")
print("predict test for Ridge")
print(defaultSummary(checkValuesRidge))
#ridge performed better than the PLS model in all 3 categories, but it's really close

#f
#all the outcome R2 values are very similar for all 3 models
