#8.5

library(knitr)
library(fpp3)
library(dplyr)
library(tsibble)
library(fable)
library(fabletools)
library(ggplot2)
library(forecast)
library(tidyverse)
library(broom)

#glimpse(global_economy)

afghanistanExports <- global_economy %>%
    filter(Code == 'AFG')


afghanPlot <- afghanistanExports %>%
    autoplot(Exports) +
    labs(title = 'Afghanistan Annual Exports')
    print(afghanPlot)

fit <- afghanistanExports %>%
    model(ETS(Exports ~ error('A') + trend('N') + season('N')))

forecastANN <- fit %>%
    forecast(h = "4 years")

afghanistanExports <- afghanistanExports %>%
    filter(!is.na(Exports))

afghanForecastPlot <- autoplot(afghanistanExports, Exports) +
    autolayer(forecastANN) +
    labs(title = "Forecast for Afghanistan Exports")
    #print(afghanForecastPlot)

#method 1 doesn't work I cant complete 8.5 because my fable or fabletool is having issues
#accuracy(fit)
fittedValues <- fitted(fit)[["ETS(Exports ~ error(\"A\") + trend(\"N\") + season(\"N\"))"]]

#method 2 manually finding the rmse and I get NaN
#remove NA
validIndices <- which(!is.na(fittedValues) & !is.na(afghanistanExports[["Exports"]]))

#finding only valid indices
fittedValues <- fittedValues[validIndices]
actualValues <- afghanistanExports[["Exports"]][validIndices]

residualsANN <- actualValues - fittedValues

rmseANN <- sqrt(mean(residualsANN^2, na.rm = TRUE))

print(rmseANN)