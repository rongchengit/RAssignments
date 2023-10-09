#8.7
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

gasProd <- aus_production %>%
    autoplot(Gas)
    print(gasProd)

fit <- aus_production %>%
    model(fit = ETS(Gas))
    report(fit)

forecastPlot <- fit %>%
    forecast(h = 4) %>%
    autoplot(aus_production)
    #print(forecastPlot)

#seasonal variation trends upwards over time

fitd <- aus_production %>%
    model(fit = ETS(Gas  ~ trend('Ad', phi = 0.9)))

dampPlot <- fitd %>%
    forecast(h = 4) %>%
    autoplot(aus_production)
    print(dampPlot)

#the trend damped doesn't look like it improved the forecast