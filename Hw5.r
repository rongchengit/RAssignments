#8.1
#install.packages("broom")
library(fpp3)
library(dplyr)
library(tsibble)
library(fable)
library(ggplot2)
library(fabletools)
library(forecast)
library(tidyverse)
library(lubridate) 
library(broom)

#filter
pigs <- aus_livestock %>% 
    filter(Animal == 'Pigs' & State == 'Victoria')

#plot
pigPlot <- pigs %>%
    autoplot(Count) +
    labs(title = 'Pigs Slaughtered in Victoria Timeseries')
    #print(pigPlot)

#----------------------------------------------------------------
# ets()
fit <- pigs %>%
    model(ses = ETS(Count ~ error('A') + trend('N') + season('N')))

#report
optimalValuesReport <- fit %>%
    report()


#forecast for next 4 months
forecastPig <- fit %>%
    fabletools::forecast(h = 4)
    # print("4month forecast")
    # print(forecastPig)

#forecast plot
forecastPigPlot <- fit %>%
    forecast(h = 4) %>%
    autoplot(filter(pigs, Month >= yearmonth('2017 Jan'))) +
    labs(title = '4 Months of Forecast for Pigs Slaughter')
    #print(forecastPigPlot)

#------------------------------------------------------------
#first forecast
yHat <- forecastPig %>%
    pull(Count) %>%
    head(1)

#STD for resid
standardDeviation <- augment(fit) %>%
    pull(.resid) %>%
    sd()

print("std calculation")
#calculate lower and upper interval
lowerCi <- yHat - 1.96 * standardDeviation
upperCi <- yHat + 1.96 * standardDeviation
results <- c(lowerCi, upperCi)
names(results) <- c('Lower', 'Upper')
print("std results")
print(results)

#hilo for comparsion
print("hilo comparsion")
print(hilo(forecastPig$Count, 95))