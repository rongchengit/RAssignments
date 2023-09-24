library(fpp3)
library(dplyr)
library(tsibble)
library(fable)
library(ggplot2)
library(fabletools)
library(forecast)
library(tidyverse)
library(lubridate) 

#glimpse(gafa_stock)

fbStock <- gafa_stock %>%
  filter(Symbol == "FB") %>%
  select(Date, Close)

#timeplot
fbPlot <- ggplot(fbStock, aes(x = Date, y = Close)) +
  geom_line() +
  labs(title = "Facebook Stock Price Over Time",
       x = "Date",
       y = "Closing Price")

#print(fbPlot)
#produce forecasts using the drift method and plot them.
fbTsibble <- fbStock %>% as_tsibble(index = Date)

fbDf <- as.data.frame(fbTsibble)

#create sequence for dates
allDates <- seq.Date(min(fbDf$Date), max(fbDf$Date), by="day")

#dataframe for sequence
allDatesDf <- data.frame(Date = allDates)

#joining to include all dates
fbComplete <- allDatesDf %>%
  left_join(fbDf, by="Date") %>%
  tidyr::fill(Close)  #fill na values with the last known

fbCompleteTsibble <- as_tsibble(fbComplete, index = Date)

#model and forecast
fitDrift <- fbCompleteTsibble %>%
  fabletools::model(fable::RW(Close ~ drift()))

forecastDrift <- fitDrift %>%
  fabletools::forecast(h = "12 months")

driftPlot <- feasts::autoplot(forecastDrift)

print(driftPlot)

first_obs <- fbComplete$Close[1]
last_obs <- fbComplete$Close[nrow(fbComplete)]
num_days <- as.numeric(difftime(max(fbComplete$Date), min(fbComplete$Date), units="days"))

slope <- (last_obs - first_obs) / num_days

forecast_data <- as.data.frame(forecastDrift)

forecast_dates <- seq.Date(from = max(fbComplete$Date) + 1, length.out = nrow(forecast_data), by = "day")

forecast_data$Date <- forecast_dates

comparisonPlot <- ggplot(fbComplete, aes(x = Date, y = Close)) +
  geom_line(color = "blue") +  # Original data in blue
  geom_line(data = forecast_data, aes(x = Date, y = `.mean`), color = "red") +  # Drift method forecast in green
  labs(title = "Facebook Stock Price Forecasts",
       x = "Date",
       y = "Closing Price")

print(comparisonPlot)

n <- nrow(fbCompleteTsibble)
train_tsibble <- fbCompleteTsibble[1:(n-30),]
validation_tsibble <- fbCompleteTsibble[(n-29):n,]

#model and training set

#drift
fitDrift_train <- train_tsibble %>%
  fabletools::model(fable::RW(Close ~ drift()))

#naive
fit_naive <- train_tsibble %>%
  fabletools::model(naive = fable::NAIVE(Close))

#snaive
fit_snaive <- train_tsibble %>%
  fabletools::model(snaive = fable::SNAIVE(Close))

#mean forecast
fit_mean <- train_tsibble %>%
  fabletools::model(meanf = fable::MEAN(Close))

#generation
h <- nrow(validation_tsibble)
forecast_drift <- fitDrift_train %>% fabletools::forecast(h = h)
forecast_naive <- fit_naive %>% fabletools::forecast(h = h)
forecast_snaive <- fit_snaive %>% fabletools::forecast(h = h)
forecast_mean <- fit_mean %>% fabletools::forecast(h = h)

#evulating forecast
accuracy_drift <- fabletools::accuracy(forecast_drift, validation_tsibble)
accuracy_naive <- fabletools::accuracy(forecast_naive, validation_tsibble)
accuracy_snaive <- fabletools::accuracy(forecast_snaive, validation_tsibble)
accuracy_mean <- fabletools::accuracy(forecast_mean, validation_tsibble)


print(accuracy_drift)
print(accuracy_naive)
print(accuracy_snaive)
print(accuracy_mean)