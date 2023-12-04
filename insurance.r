library(dplyr)
library(tidyr)
library(stats)
library(caret)
library(pROC)


#data import
trainingData <- read.csv("/Users/rongchen/Desktop/hw4/insTraining.csv")
evalData <- read.csv("/Users/rongchen/Desktop/hw4/insEval.csv")

#converted strings into numeric values from the data
convertCurrencyToNumeric <- function(df, columns) {
  for (column in columns) {
    if (is.character(df[[column]])) {
      df[[column]] <- as.numeric(gsub("[\\$,]", "", df[[column]], fixed = FALSE))
    }
  }
  return(df)
}

#calculate the mode
getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Data prepping
#converting strings pt 2
#replacing missing NA data with mode
preprocessData <- function(df) {
  #currency columns to numeric
  currencyCols <- c('INCOME', 'HOME_VAL', 'BLUEBOOK', 'OLDCLAIM')
  df <- convertCurrencyToNumeric(df, currencyCols)
  
  #replacing NA
  for (col in names(df)) {
    if (is.numeric(df[[col]])) {
      df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
    } else {
      df[[col]] <- as.factor(df[[col]])
      df[[col]][is.na(df[[col]])] <- as.character(getMode(df[[col]]))
    }
  }
  df <- df %>%
    mutate_if(is.factor, as.numeric)

  return(df)
}
#transfering the data
trainingDataPreprocessed <- preprocessData(trainingData)

#Creating Models
#splitting data into training and test sets
set.seed(42)
splitIndex <- createDataPartition(trainingDataPreprocessed$TARGET_FLAG, p = 0.8, list = FALSE)
trainSet <- trainingDataPreprocessed[splitIndex, ]
testSet <- trainingDataPreprocessed[-splitIndex, ]

trainSetComplete <- trainSet[complete.cases(trainSet[c("TARGET_AMT", "AGE", "INCOME", "HOME_VAL", "CAR_AGE")]), ]

#model 1: linear regression using age, income, home valuation, and car age for target amount
lmFit1 <- lm(TARGET_AMT ~ AGE + INCOME + HOME_VAL + CAR_AGE, data = trainSetComplete)
print("Linear Model 1 Summary:")
print(summary(lmFit1))

#model 2: linear regression using poly age and income for target amount
lmFit2 <- lm(TARGET_AMT ~ poly(AGE, 2) + poly(INCOME, 2) + HOME_VAL + CAR_AGE, data = trainSetComplete)
print("Linear Model 2 Summary:")
print(summary(lmFit2))

#binary logistic regression for TARGET_FLAG

#model 1: logistic regression with age income car type, mvr pts for target_flag
glmFit1 <- glm(TARGET_FLAG ~ AGE + INCOME + CAR_TYPE + MVR_PTS, data = trainSet, family = "binomial")
print("Logistic Model 1 Summary:")
print(summary(glmFit1))

#model 2: homekids, job experience, age of car, previous claims 
glmFit2 <- glm(TARGET_FLAG ~ HOMEKIDS + YOJ + CAR_AGE + OLDCLAIM, data = trainSet, family = "binomial")
print("Logistic Model 2 Summary:")
print(summary(glmFit2))

#model 3: log age, squared income, car and mvrpts measurements
glmFit3 <- glm(TARGET_FLAG ~ log(AGE) + sqrt(INCOME) + CAR_TYPE:MVR_PTS, data = trainSet, family = "binomial")
print("Logistic Model 3 Summary:")
print(summary(glmFit3))

#model selection
evaluateLogisticModel <- function(model, data, threshold = 0.5) {
  predictions <- predict(model, data, type = "response")

  #roc and auc
  rocInfo <- roc(data$TARGET_FLAG, predictions)
  aucValue <- auc(rocInfo)

  #confusion maxtrix
  confusionMatrixData <- confusionMatrix(as.factor(ifelse(predictions > threshold, 1, 0)), as.factor(data$TARGET_FLAG))

  list(AUC = aucValue, ConfusionMatrix = confusionMatrixData)
}

#model1
evaluation1 <- evaluateLogisticModel(glmFit1, trainSet)
print("Logistic Model 1 Evaluation:")
print(evaluation1)

#model2
evaluation2 <- evaluateLogisticModel(glmFit2, trainSet)
print("Logistic Model 2 Evaluation:")
print(evaluation2)

#model3
evaluation3 <- evaluateLogisticModel(glmFit3, trainSet)
print("Logistic Model 3 Evaluation:")
print(evaluation3)