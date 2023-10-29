library(forecast)
library(readxl)
library(ggplot2)

rsForecastData <- read_excel("/Users/rongchen/Desktop/Project 1 624/rsforecast.xlsx")

kwhTimeSeries <- ts(rsForecastData$KWH, start=c(1998,1), frequency=12)

bestFit <- auto.arima(kwhTimeSeries)

forecastValues <- forecast(bestFit, h=12)

print("Forecasted KWH values for 2014:")
print(forecastValues)

#dataframe
forecastDf <- as.data.frame(forecastValues)

#combining 
dataCombined <- data.frame(Date = c(time(kwhTimeSeries), time(forecastValues$mean)),
                           KWH = c(as.numeric(kwhTimeSeries), as.numeric(forecastValues$mean)))

#plot
kwhPlot <- ggplot(dataCombined, aes(x = Date, y = KWH)) + 
  geom_line(color = "black") + 
  geom_point(data = dataCombined[(length(kwhTimeSeries)+1):nrow(dataCombined),], 
             aes(x = Date, y = KWH), color = "red") +
  labs(title = "KWH Actual vs. Forecasted", 
       x = "Date", 
       y = "KWH") + 
  theme_minimal()

print(kwhPlot)