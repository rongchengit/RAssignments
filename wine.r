library(tidyverse)
library(MASS)
library(caret)
set.seed(123)

#dataset
wineData <- read.csv("/Users/rongchen/Desktop/hw5/wine-training-data.csv")

#Data Exploration
#print(summary(wineData))
#----------------------------------------------------------------

#Data Preparation
# Checking for missing values
missingValues <- sapply(wineData, function(x) sum(is.na(x)))

#using mediam for NA values
wineData$FreeSulfurDioxide[is.na(wineData$FreeSulfurDioxide)] <- median(wineData$FreeSulfurDioxide, na.rm = TRUE)
wineData$Alcohol[is.na(wineData$Alcohol)] <- median(wineData$Alcohol, na.rm = TRUE)

#flagging NA values
wineData$AlcoholFlag <- ifelse(is.na(wineData$Alcohol), 1, 0)

#spliting testing and training data
index <- createDataPartition(wineData$TARGET, p = 0.8, list = FALSE)
trainData <- wineData[index, ]
testData <- wineData[-index, ]

#----------------------------------------------------------------
#Build Models
#Poisson regression models
poissonModel1 <- glm(TARGET ~ ., family = poisson(link = "log"), data = trainData)
poissonModel2 <- glm(TARGET ~ FixedAcidity + VolatileAcidity, family = poisson(link = "log"), data = trainData)

print("poissonModel1")
print(summary(poissonModel1))
print("poissonModel2")
print(summary(poissonModel2))

#Negative binomial regression models
nbModel1 <- glm.nb(TARGET ~ ., data = trainData)
nbModel2 <- glm.nb(TARGET ~ FixedAcidity + VolatileAcidity, data = trainData)

print("negativeBinomialModel1")
print(summary(nbModel1))
print("negativeBinomialModel2")
print(summary(nbModel2))

#linear regression models
linearModel1 <- lm(TARGET ~ ., data = trainData)
linearModel2 <- lm(TARGET ~ FixedAcidity + VolatileAcidity, data = trainData)

print("linearModel1")
print(summary(linearModel1))
print("linearModel2")
print(summary(linearModel2))

#----------------------------------------------------------------
#model selection
aicValues <- c(AIC(poissonModel1), AIC(poissonModel2), AIC(nbModel1), AIC(nbModel2), AIC(linearModel1), AIC(linearModel2))
names(aicValues) <- c("Poisson Model 1", "Poisson Model 2", "NB Model 1", "NB Model 2", "Linear Model 1", "Linear Model 2")

#check for overdispersion in Poisson models
overdispersionTest1 <- sum(residuals(poissonModel1, type = "pearson")^2) / poissonModel1$df.residual
overdispersionTest2 <- sum(residuals(poissonModel2, type = "pearson")^2) / poissonModel2$df.residual

#adjust AIC values for overdispersion
if (overdispersionTest1 > 1.5) {
  aicValues["Poisson Model 1"] <- AIC(nbModel1)
}
if (overdispersionTest2 > 1.5) {
  aicValues["Poisson Model 2"] <- AIC(nbModel2)
}

selectedModelName <- names(which.min(aicValues))
if (selectedModelName == "Poisson Model 1") {
  selectedModel <- poissonModel1
} else if (selectedModelName == "Poisson Model 2") {
  selectedModel <- poissonModel2
} else if (selectedModelName == "NB Model 1") {
  selectedModel <- nbModel1
} else if (selectedModelName == "NB Model 2") {
  selectedModel <- nbModel2
} else if (selectedModelName == "Linear Model 1") {
  selectedModel <- linearModel1
} else if (selectedModelName == "Linear Model 2") {
  selectedModel <- linearModel2
}

#print the selected model's summary
print(paste("Selected Model:", selectedModelName))
print(summary(selectedModel))

#make predictions using the selected model on the evaluation
predictions <- predict(selectedModel, newdata = testData, type = "response")

#print performance metrics
rmse <- sqrt(mean((testData$TARGET - predictions)^2))
mae <- mean(abs(testData$TARGET - predictions))
mape <- mean(abs((testData$TARGET - predictions) / testData$TARGET), na.rm = TRUE)

print(paste("Root Mean Squared Error (RMSE):", rmse))
print(paste("Mean Absolute Error (MAE):", mae))
print(paste("Mean Absolute Percentage Error (MAPE):", mape))