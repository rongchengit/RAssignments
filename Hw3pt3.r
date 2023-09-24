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

ausExports <- global_economy %>% 
  filter(Code == "AUS") %>% 
  select(Year, Exports)

exportPlot <- ggplot(ausExports, aes(x = Year, y = Exports)) + 
  geom_line() +
  ggtitle("Australian Exports")

print(exportPlot)

#no season so I used naive
fitExports <- ausExports %>% model(NAIVE(Exports))
forecastsExports <- fitExports %>% forecast()
exportForecastPlot <- autoplot(ausExports, Exports) + 
  autolayer(forecastsExports) +
  ggtitle("NAIVE Forecast for Australian Exports")

#print(exportForecastPlot)

#bricksplot
brickPlot <- ggplot(aus_production, aes(x = Quarter, y = Bricks)) + 
  geom_line() +
  ggtitle("Australian Bricks Production")

#print(brickPlot)

#seasonal used snaive
fitBricks <- aus_production %>% model(SNAIVE(Bricks))
forecastsBricks <- fitBricks %>% forecast()
brickForecastPlot <- autoplot(aus_production, Bricks) + 
  autolayer(forecastsBricks) +
  ggtitle("SNAIVE Forecast for Australian Bricks Production")

#print(brickForecastPlot)