#Rong Chen
#Homework 1
#Dr.Nasrin Khansari
#packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(corrplot)
library(lmtest)
library(e1071)

# Data import
moneyballEval <- read.csv("/Users/rongchen/Desktop/Data Mining Homework 1/moneyball.csv")
moneyballTraining <- read.csv("/Users/rongchen/Desktop/Data Mining Homework 1/moneyballTraining.csv")

#-Data Preparation-------------------------------------------------------------
#-Data Cleaning----------------------------------------------------------------

#opted to use this over removing NA or blanks to avoid losing important information
#and it works better with a log transformation
# Fix missing values with Mean
moneyballTraining <- moneyballTraining %>%
  mutate_all(function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# print(paste("Number of NAs in the dataset:", sum(is.na(moneyballTraining))))

#-Data exploration----------------------------------------------------------------

# #data set description 
# print(paste("Number of rows in the dataset:", nrow(moneyballTraining)))
# print(paste("Number of columns in the dataset:", ncol(moneyballTraining)))

# print("Variables in the dataset:")
# print(colnames(moneyballTraining))

#barchart for mean, median, standard deviation of selected variables
#i created subsets so it can be done on the barchart
selectedVars <- c("TARGET_WINS", "TEAM_BATTING_H", "TEAM_BATTING_2B", "TEAM_BATTING_3B", 
                  "TEAM_BATTING_HR", "TEAM_BATTING_BB", "TEAM_BATTING_SO", "TEAM_BASERUN_SB", 
                  "TEAM_BASERUN_CS", "TEAM_BATTING_HBP", "TEAM_PITCHING_H", "TEAM_PITCHING_HR", 
                  "TEAM_PITCHING_BB", "TEAM_PITCHING_SO", "TEAM_FIELDING_E", "TEAM_FIELDING_DP")

subMeans <- sapply(moneyballTraining[selectedVars], mean)
subSds <- sapply(moneyballTraining[selectedVars], sd)
subMedians <- sapply(moneyballTraining[selectedVars], median)

#barchart
# barChart <- barplot(rbind(subMeans, subSds, subMedians), beside=TRUE, 
#                     col=c("blue", "red", "green"), las=2, 
#                     ylim=c(0, max(c(subMeans, subSds, subMedians)) + 10), 
#                     main="Bar Chart for Selected Stats", cex.names=0.7)
# legend("topright", legend = c("Mean", "SD", "Median"), fill = c("blue", "red", "green"))

#-Data Prepping----------------------------------------------------------------
#opted to use log transformation to get a better stablization of the data
#since I went ahead and basically added averages for it.
#this works better than taking out the data as the line can kinda of adjust better for regression
# Log transforms
moneyballTrainingLog <- moneyballTraining %>%
  mutate_at(vars(-INDEX, -TARGET_WINS), ~log(. + 1))

# print("log Transformation")
# print(head(moneyballTrainingLog))

#-Data Models----------------------------------------------------------------
#model using all available statistics
#31.92% of the variation in wins for the r-squared
modelAllVars <- lm(TARGET_WINS ~ . - INDEX, data = moneyballTraining)
print("model 1")
print(summary(modelAllVars))

# Model using selected variables because these 4 are probably most important in baseball when it comes to winning aka scoring
# TEAM_BATTING_H (Team Batting Hits) - teams that hit well might score more runs
# TEAM_BATTING_HR (Team Batting Home Runs) - home runs are direct scores
# TEAM_PITCHING_H (Team Pitching Hits) - fewer hits allowed by pitchers means the opposing team scores fewer runs
# TEAM_PITCHING_SO (Team Pitching Strikeouts) - more strikeouts by a pitcher mean fewer scoring opportunities for the opposing team
# 22.97% of the wins for the r-squared
modelSelectedVars <- lm(TARGET_WINS ~ TEAM_BATTING_H + TEAM_BATTING_HR + TEAM_PITCHING_H + TEAM_PITCHING_SO, data = moneyballTraining)
print("model 2")
print(summary(modelSelectedVars))

#model using logs data
#22.43% of the wins for the r-squared
#transformation also captures the idea that the relationship between hits and wins might be non linear
#the variables chosen focuseson specific predictors that might be more influential or interpretable
modelTransformedVars <- lm(TARGET_WINS ~ log(TEAM_BATTING_H + 1) + sqrt(TEAM_BATTING_HR) + TEAM_PITCHING_H + TEAM_FIELDING_E, data = moneyballTrainingLog)
print("model 3")
print(summary(modelTransformedVars))

#-Predictions--------------------------------
#for finding the highest predictive accuracy for simple models like the ones I created
#metrics like adjusted r2 is the one I am mostly looking at
#otherwise I would look at RMSE if it was more complex

#so the run down for each one of the models are
#model 1 has the highest predictive accuracy, however its the most random and also the most complex that including non signifcant predictors
#model 2 doesn't have the highest predictive accuracy, but all its predictors are significant, so it's more reliable and easy to understand
#model 3 is the lowest and for that one I was mainly looking for predictors that could be more signifcant after a log/sqrt change

#model 2 I would say is the best one to use
#I also tested out removing data, creating/combining variables for new variables 
#like hrHitRatio (Home Run to Hit Ratio) and soBbRatio (Strikeout to Walk Ratio for Pitching) as those are important to baseball
#as well, but it came out more complex than I had hope I got rid of them

#-data cleaning for evaluation--------------------------------
moneyballEval <- moneyballEval %>%
  mutate_all(function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

print(paste("Number of NAs in the evaluation dataset:", sum(is.na(moneyballEval))))

#predict
predictedWins <- predict(modelSelectedVars, newdata = moneyballEval)

print("Predicted Wins for the evaluation")
print(predictedWins)