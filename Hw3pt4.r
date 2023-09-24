library(fpp3)
library(dplyr)
library(tsibble)
library(fable)
library(ggplot2)
library(fabletools)
library(forecast)
library(tidyverse)
library(lubridate) 
library(feasts)


#glimpse(aus_retail)
ausRetail <- aus_retail %>%
  filter(Industry == "Department stores") %>%
  select(Month, Turnover)

# spliting training and test
ausRetailTrain <- ausRetail %>% 
  filter(year(Month) < 2011)

#snaive
fit <- ausRetailTrain %>%
  model(SNAIVE = SNAIVE(Turnover))

#insample
inSampleForecast <- augment(fit) %>%
  as_tibble()

trainAccuracy <- accuracy(inSampleForecast$.fitted, ausRetailTrain$Turnover)
print("trainingAccuracy")
print(trainAccuracy)

#forecast test data
fc <- fit %>%
  forecast(new_data = anti_join(ausRetail, ausRetailTrain, by = "Month"))

#accuray test
testAccuracy <- accuracy(fc$.mean, anti_join(ausRetail, ausRetailTrain, by = "Month")$Turnover)
print("testAccuracy")
print(testAccuracy)

#plot actual vs values
forecastVsActualPlot <- fc %>%
  autoplot(ausRetail) +
  ggtitle("ForecastVsActualValues")
print(forecastVsActualPlot)
