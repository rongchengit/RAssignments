
library(readxl)
library(forecast)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(zoo)

atmData <- read_excel("/Users/rongchen/Desktop/Project 1 624/ATM624Data.xlsx")

#date conversion
atmData$DATE <- as.Date(atmData$DATE, origin = "1899-12-30")

#filtering out na, but not zero
atmData <- atmData %>% filter(!is.na(Cash))

#spliting atm data
atm1Data <- atmData %>% filter(ATM == "ATM1")
atm2Data <- atmData %>% filter(ATM == "ATM2")
atm3Data <- atmData %>% filter(ATM == "ATM3")
atm4Data <- atmData %>% filter(ATM == "ATM4")

#Data Visual for each ATM----------------------------------------------------------------
# print(atm1Data %>% ggplot(aes(x = DATE, y = Cash)) + geom_line() + ggtitle("ATM1 Cash Over Time"))

# print(atm2Data %>% ggplot(aes(x = DATE, y = Cash)) + geom_line() + ggtitle("ATM2 Cash Over Time"))

# print(atm3Data %>% ggplot(aes(x = DATE, y = Cash)) + geom_line() + ggtitle("ATM3 Cash Over Time"))

# print(atm4Data %>% ggplot(aes(x = DATE, y = Cash)) + geom_line() + ggtitle("ATM4 Cash Over Time"))

#----------------------------------------------------------------
#timeSeries conversion for 12 months
tsAtm1 <- ts(atm1Data$Cash, start = c(year(min(atm1Data$DATE)), month(min(atm1Data$DATE))), frequency = 12)
tsAtm2 <- ts(atm2Data$Cash, start = c(year(min(atm2Data$DATE)), month(min(atm2Data$DATE))), frequency = 12)
tsAtm3 <- ts(atm3Data$Cash, start = c(year(min(atm3Data$DATE)), month(min(atm3Data$DATE))), frequency = 12)
tsAtm4 <- ts(atm4Data$Cash, start = c(year(min(atm4Data$DATE)), month(min(atm4Data$DATE))), frequency = 12)

#arima
modelAtm1 <- auto.arima(tsAtm1)
modelAtm2 <- auto.arima(tsAtm2)
modelAtm3 <- auto.arima(tsAtm3)
modelAtm4 <- auto.arima(tsAtm4)

#may forecast
forecastAtm1 <- forecast(modelAtm1, h = 1)
forecastAtm2 <- forecast(modelAtm2, h = 1)
forecastAtm3 <- forecast(modelAtm3, h = 1)
forecastAtm4 <- forecast(modelAtm4, h = 1)

#average
cat("Forecasted Cash Taken Out in May 2010:\n")
cat("ATM1:", forecastAtm1$mean, "\n")
cat("ATM2:", forecastAtm2$mean, "\n")
cat("ATM3:", forecastAtm3$mean, "\n")
cat("ATM4:", forecastAtm4$mean, "\n")

#----------------------------------------------------------------

#combining average with og data
extendDataWithForecast <- function(data, forecasted){
  lastDate <- max(data$DATE)
  newDate <- lastDate + lubridate::days(1)
  extendedData <- rbind(data, data.frame(DATE = newDate, Cash = forecasted$mean, ATM = unique(data$ATM)))
  return(extendedData)
}

atm1Data <- extendDataWithForecast(atm1Data, forecastAtm1)
atm2Data <- extendDataWithForecast(atm2Data, forecastAtm2)
atm3Data <- extendDataWithForecast(atm3Data, forecastAtm3)
atm4Data <- extendDataWithForecast(atm4Data, forecastAtm4)

#graph
visualizeData <- function(data, title){
  p <- ggplot(data, aes(x = DATE, y = Cash)) + 
    geom_line() + 
    geom_point(aes(color = ifelse(DATE == max(data$DATE), "Forecast", "OG")), size = 3) +
    ggtitle(title) + 
    scale_color_manual(values = c("OG" = "black", "Forecast" = "red")) +
    theme_minimal() +
    labs(color = "")
  return(p)
}

p1 <- visualizeData(atm1Data, "ATM1 Cash Over Time with Forecast")
p2 <- visualizeData(atm2Data, "ATM2 Cash Over Time with Forecast")
p3 <- visualizeData(atm3Data, "ATM3 Cash Over Time with Forecast")
p4 <- visualizeData(atm4Data, "ATM4 Cash Over Time with Forecast")

# print(p1)
# print(p2)
# print(p3)
print(p4)
