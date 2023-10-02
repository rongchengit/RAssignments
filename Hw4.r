
library(mlbench)
library(tidyr)
library(ggplot2)
library(corrplot)
library(GGally)
library(caret)

#load data set
data(Glass) 
#load structure
str(Glass) 

#visualize distributions
histPlot <- Glass %>%
  gather(key = "predictor", value = "value", -Type) %>%#not including type column
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "dodgerblue") +
  facet_wrap(~ predictor, scales = "free") +
  labs(title = "Histogram of Predictor Variables")
  
print(histPlot)

scatterMatrix <- Glass %>%
  purrr::keep(is.numeric) %>%
  ggpairs(title = "Scatterplot Matrix of Predictor Variables")

print(scatterMatrix)
#based on the graph all, yeah all variables are skewed and have outliers

#box cut is pretty good for skewed data, just to even it out 
preBoxCox <- preProcess(Glass[,-10], method = "BoxCox")
glassBoxCoxed <- predict(preBoxCox, Glass[,-10])

histBoxCoxed <- glassBoxCoxed %>%
  gather(key = "predictor", value = "value") %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "dodgerblue") +
  facet_wrap(~ predictor, scales = "free") +
  labs(title = "Histogram of Box-Cox Transformed Predictor Variables")

print(histBoxCoxed)