library(e1071)
library(ggplot2)
library(corrplot)
library(ROCR)

crimeEval <- read.csv("/Users/rongchen/Desktop/hw3/crimeEval.csv")
crimeTrainings <- read.csv("/Users/rongchen/Desktop/hw3/crimeTraining.csv")

# print("crimeEval")
# glimpse(crimeEval)
# print("crimeTrainings")
# glimpse(crimeTrainings)

#data exploration
#creating a summary for the data 
print("summary")
print(summary(crimeTrainings))

#showcasing a heatmap for which variables correlate with each other
#with color definement and size indicating how strong the relationship is for each variable
#blue circles indicate positive correlation
#red circles indicate negative correlation
#white circles indicate little to no correlation    
corMatrix <- cor(crimeTrainings[, -13])
print(corMatrix)
corrplot(corMatrix, method = "circle")

#Data prepping, just getting rid of missing data
missingValues <- sum(is.na(crimeTrainings))
print(paste("Number of missing values:", missingValues))

if(missingValues > 0) {
  crimeTrainings <- na.omit(crimeTrainings)
}
#data modeling
#3 three different binary logistic regression models

#uses all the data
#AUC: 0.974
model1 <- glm(target ~ ., family = binomial(link = "logit"), data = crimeTrainings)
print("model1")
print(summary(model1))

#using only zn, indus, and nox because they have strong relationships with each other
#it is is looking at the type of buildings in the area, and the level of pollution and how these might relate to crime
#AUC: 0.935
model2 <- glm(target ~ zn + indus + nox, family = binomial(link = "logit"), data = crimeTrainings)
print("model2")
print(summary(model2))

#using only rm, age, and tax because they have strong relationships with each other
#is considering the size and age of homes and how much people pay in property taxes to understand crime rates
#AUC: 0.912
model3 <- glm(target ~ rm + age + tax, family = binomial(link = "logit"), data = crimeTrainings)
print("model3")
print(summary(model3))

#Metrics
computeMetrics <- function(actual, predicted) {
  confusion <- table(actual, predicted)
  TN <- confusion[1,1]
  FP <- confusion[1,2]
  FN <- confusion[2,1]
  TP <- confusion[2,2]
  
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  errorRate <- 1 - accuracy
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  f1Score <- 2 * (precision * recall) / (precision + recall)
  
  metrics <- list(accuracy = accuracy, errorRate = errorRate, precision = precision,
                  recall = recall, specificity = specificity, f1Score = f1Score,
                  confusionMatrix = confusion)
  return(metrics)
}

#AUC 
predictedProbabilities1 <- predict(model1, type = "response")
predictedClass1 <- ifelse(predictedProbabilities1 > 0.5, 1, 0)
metrics1 <- computeMetrics(crimeTrainings$target, predictedClass1)

predictedProbabilities2 <- predict(model2, type = "response")
predictedClass2 <- ifelse(predictedProbabilities2 > 0.5, 1, 0)
metrics2 <- computeMetrics(crimeTrainings$target, predictedClass2)

predictedProbabilities3 <- predict(model3, type = "response")
predictedClass3 <- ifelse(predictedProbabilities3 > 0.5, 1, 0)
metrics3 <- computeMetrics(crimeTrainings$target, predictedClass3)

pred1 <- prediction(predictedProbabilities1, crimeTrainings$target)
auc1 <- performance(pred1, measure = "auc")@y.values[[1]]

pred2 <- prediction(predictedProbabilities2, crimeTrainings$target)
auc2 <- performance(pred2, measure = "auc")@y.values[[1]]

pred3 <- prediction(predictedProbabilities3, crimeTrainings$target)
auc3 <- performance(pred3, measure = "auc")@y.values[[1]]

#the auc is the model that tells us how well a model performs from 0-1 so the closer to 1 the better
print(paste("Model 1 AUC:", round(auc1, 3)))
print(paste("Model 2 AUC:", round(auc2, 3)))
print(paste("Model 3 AUC:", round(auc3, 3)))

#model selection
#so while certain variables are related to higher crime rates, it doesn't mean they cause them
#model 1 is the most accurate with all variables included, and I think for good reasons because for something
#as important as crime, it's best not to leave out any variables, whether they are super siginifcant like nox 
#or barely any significant like chas