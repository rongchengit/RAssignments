# 9.1, 9.2, 9.3, 9.5, 9.6, 9.7, 9.8 in Hyndman
library(fpp3)
library(forecast)
library(ggplot2)
library(tseries)
library(dplyr)
library(tsibble)

# print(head(gafa_stock))
# print(head(aus_accommodation))
# print(head(souvenirs))

amznStock <- gafa_stock %>%
  filter(Symbol == "AMZN")

amazonDisplayPlot <- amznStock %>%
  gg_tsdisplay(Close, plot_type = 'partial') +
  labs(title= "Daily Closing Prices for Amazon", subtitle= "Ticker: AMZN")

print("Time Series Display of Daily Closing Prices, ACF, and PACF for Amazon:")
print(amazonDisplayPlot)

print("Unit Root Test Results:")
print(amznStock %>%
  features(Close, unitroot_kpss))

#9.3----------------------------------------------------------------
turkishEconomy <- global_economy %>% 
  filter(Country == 'Turkey') %>% 
  select(GDP)

lambdaGDP <- forecast::BoxCox.lambda(turkishEconomy$GDP)
turkishGdpTransformed <- forecast::BoxCox(turkishEconomy$GDP, lambdaGDP)

tasmaniaTakings <- aus_accommodation %>% 
  filter(State == "Tasmania")

lambdaTas <- forecast::BoxCox.lambda(tasmaniaTakings$Takings)
tasmaniaTransformed <- forecast::BoxCox(tasmaniaTakings$Takings, lambdaTas)

lambdaSales <- forecast::BoxCox.lambda(souvenirs$Sales)
salesTransformed <- forecast::BoxCox(souvenirs$Sales, lambdaSales)

cat("Lambda for Turkish GDP:", lambdaGDP, "\n")
cat("Lambda for Tasmania takings:", lambdaTas, "\n")
cat("Lambda for souvenirs sales:", lambdaSales, "\n")