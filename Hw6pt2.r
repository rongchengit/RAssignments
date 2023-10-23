
library(urca)
library(fpp3)
library(forecast)
library(ggplot2)
library(tseries)
library(dplyr)
library(tsibble)

#9.7
fit <- aus_airpassengers %>%
  model(ARIMA(Passengers))

report_fit <- report(fit)
# print(report_fit)

residualPlot <- fit %>%
  gg_tsresiduals() +
  labs(title = 'Australian Air Passengers Residual Checking')
# print(residualPlot)

forecastPlot <- fit %>%
  forecast(h = 10) %>%
  autoplot(aus_airpassengers) +
  labs(title = 'Australian Air Passengers with 10-year forecast')
# print(forecastPlot)

# #----------------------------------------------------------------
fit2 <- aus_airpassengers %>%
  model(ARIMA(Passengers ~ pdq(0, 1, 0)))

# Forecasting with ARIMA(0,1,0) Model
forecastPlotArima010 <- fit2 %>%
  forecast(h = 10) %>%
  autoplot(aus_airpassengers) +
  labs(title = 'Australian Air Passengers with 10-year forecast from an ARIMA(0,1,0) model')
# print(forecastPlotArima010)

# #----------------------------------------------------------------
fit3a <- aus_airpassengers %>%
  model(ARIMA(Passengers ~ 1 + pdq(2, 1, 2)))
# Forecasting with ARIMA(2,1,2) Model

forecastPlotArima212 <- fit3a %>%
  forecast(h = 10) %>%
  autoplot(aus_airpassengers) +
  labs(title = 'Australian Air Passengers with 10-year forecast from an ARIMA(2,1,2) model')
# print(forecastPlotArima212)

# #----------------------------------------------------------------
fit3b <- aus_airpassengers %>%
  model(ARIMA(Passengers ~ 0 + pdq(2, 1, 2)))

forecastPlotArima212NoConstant <- fit3b %>%
  forecast(h = 10) %>%
  autoplot(aus_airpassengers) +
  labs(title = 'Australian Air Passengers with 10-year forecast from an ARIMA(2,1,2) model without the constant')
# print(forecastPlotArima212NoConstant)

# #----------------------------------------------------------------
fit4 <- aus_airpassengers %>%
  model(ARIMA(Passengers ~ 1 + pdq(0, 2, 1)))
  
# Forecasting with ARIMA(0,2,1) Model
forecastPlotArima021 <- fit4 %>%
  forecast(h = 10) %>%
  autoplot(aus_airpassengers) +
  labs(title = 'Australian Air Passengers with 10-year forecast from an ARIMA(0,2,1) model')
# print(forecastPlotArima021)

# #9.8----------------------------------------------------------------
usGdp <- global_economy %>%
  filter(Country == "United States") %>%
  select(Country, GDP)

gdpPlot <- usGdp %>%
  autoplot(GDP) +
  labs(title = "United States GDP")
print(gdpPlot)

fitUsGdp <- usGdp %>%
  model(
    arimaModel = ARIMA(GDP, stepwise = FALSE, approx = FALSE)
  )

reportFitUsGdp <- report(fitUsGdp)
print(reportFitUsGdp)