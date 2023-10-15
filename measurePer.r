
library(caret)
library(pROC)

dfData <- read.csv("/Users/rongchen/Desktop/data.csv")

#2
confusionMatrix <- table(dfData$class, dfData$scored.class)
print("Confusion Matrix")
print(confusionMatrix)
#119: True Negatives TN
#5: False Positives FP
#30: False Negatives FN
#27: True Positives TP

#3  
calculateAccuracy <- function(data) {
  confusionMatrix <- table(data$class, data$scored.class)
  TP <- confusionMatrix[2,2]
  TN <- confusionMatrix[1,1]
  FP <- confusionMatrix[1,2]
  FN <- confusionMatrix[2,1]
  accuracy <- (TP + TN) / (TP + FP + TN + FN)
  return(accuracy)
}

#4
calculateErrorRate <- function(data) {
  confusionMatrix <- table(data$class, data$scored.class)
  TP <- confusionMatrix[2,2]
  TN <- confusionMatrix[1,1]
  FP <- confusionMatrix[1,2]
  FN <- confusionMatrix[2,1]
  errorRate <- (FP + FN) / (TP + FP + TN + FN)
  return(errorRate)
}

#4
sumCheck <- accuracyResult + errorRateResult

#5
calculatePrecision <- function(data) {
  confusionMatrix <- table(data$class, data$scored.class)
  TP <- confusionMatrix[2,2]
  FP <- confusionMatrix[1,2]
  precision <- TP / (TP + FP)
  return(precision)
}

#6
calculateSensitivity <- function(data) {
  confusionMatrix <- table(data$class, data$scored.class)
  TP <- confusionMatrix[2,2]
  FN <- confusionMatrix[2,1]
  sensitivity <- TP / (TP + FN)
  return(sensitivity)
}

#7
calculateSpecificity <- function(data) {
  confusionMatrix <- table(data$class, data$scored.class)
  TN <- confusionMatrix[1,1]
  FP <- confusionMatrix[1,2]
  specificity <- TN / (TN + FP)
  return(specificity)
}

#8
calculateF1Score <- function(data) {
  precision <- calculatePrecision(data)
  sensitivity <- calculateSensitivity(data)
  f1Score <- 2 * (precision * sensitivity) / (precision + sensitivity)
  return(f1Score)
}

#9 Proving F1 Bound is from 0 and 1-----------------------------------------------
#find sequences
precisionValues <- seq(0, 1, 0.01)
sensitivityValues <- seq(0, 1, 0.01)

#generate for all f1 combos
calculateF1 <- function(precision, sensitivity) {
  if (precision + sensitivity == 0) {
    return(0)
  }
  return(2 * precision * sensitivity / (precision + sensitivity))
}
f1Values <- mapply(calculateF1, rep(precisionValues, each=length(sensitivityValues)), rep(sensitivityValues, time=length(precisionValues)))

print(paste("Minimum F1:", min(f1Values)))
print(paste("Maximum F1:", max(f1Values)))
#------------------------------------------------------------------------
#10
generateROC <- function(data) {
  thresholds <- seq(0, 1, 0.01)
  tprList <- numeric(length(thresholds))
  fprList <- numeric(length(thresholds))
  
  for (i in 1:length(thresholds)) {
    threshold <- thresholds[i]
    
    #true and false calcs
    tp <- sum(data$class == 1 & data$scored.probability >= threshold)
    fp <- sum(data$class == 0 & data$scored.probability >= threshold)
    fn <- sum(data$class == 1 & data$scored.probability < threshold)
    tn <- sum(data$class == 0 & data$scored.probability < threshold)
    
    tpr <- tp / (tp + fn)
    fpr <- fp / (fp + tn)
    
    tprList[i] <- tpr
    fprList[i] <- fpr
  }
  
  aucValue <- sum(diff(fprList) * (head(tprList, -1) + tail(tprList, -1))) / 2
  
  plot(fprList, tprList, type="l", xlab="False Positive Rate", ylab="True Positive Rate",
       main=sprintf("ROC Curve (AUC = %.2f)", aucValue))
  return(list(rocData = data.frame(fprList, tprList), auc = aucValue))
}
rocResults <- generateROC(dfData)

#11
print(paste("Accuracy Result:", calculateAccuracy(dfData))) 
print(paste("Error Rate Results:", calculateErrorRate(dfData))) 
print(paste("Sum Check:", sumCheck))
print(paste("Precision:", calculatePrecision(dfData)))
print(paste("Sensitivity:", calculateSensitivity(dfData)))
print(paste("Specificity:", calculateSpecificity(dfData)))
print(paste("F1 Score:", calculateF1Score(dfData)))
print(paste("AUC:", rocResults$auc))
#12 caret library--------------------------------------------------------
#combine unique levels
allLevels <- unique(c(as.character(dfData$scored.class), as.character(dfData$class)))

#convert columns to same level
dfData$scored.class <- factor(dfData$scored.class, levels = allLevels)
dfData$class <- factor(dfData$class, levels = allLevels)

#caret confusion matrixs
caretCM <- confusionMatrix(dfData$scored.class, dfData$class)
print("Caret Confusion Matrix")
print(caretCM)

print(paste("Caret Sensitivity:", sensitivity(dfData$scored.class, dfData$class)))
print(paste("Caret Specificity:", specificity(dfData$scored.class, dfData$class)))

#all results are the same except false positives, and false negatives just based on how caret has their functions written
#so the sensitivity and specificity are different, but would be the same if it was wrriten like mine
#13 Proc--------------------------------------------------------   

procRoc <- roc(dfData$class, dfData$scored.probability)
plot(procRoc, main=sprintf("ROC Curve (AUC = %.2f)", auc(procRoc)))

#graphs are the same too