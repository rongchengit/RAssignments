#8.6
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


chineseGDP <- global_economy %>%
    filter(Country == 'China')

# Create a plot of the data.
chinaPlot <- chineseGDP %>% autoplot(GDP) +
    labs(title = 'Chinese GDP')
    print(chinaPlot)

lambda <- chineseGDP %>%
    features(GDP, features = guerrero) %>%
    pull(lambda_guerrero)

chineseGDPModelComparison <- chineseGDP %>%
    model(
        ETS = ETS(GDP),
        ETSBoxCox = ETS(box_cox(GDP, lambda)),
        ETSDamped = ETS(GDP ~ trend('Ad', phi = 0.9)),
        ETSLog = ETS(log(GDP))
    )

chineseGDPModelComparisonForecast <- chineseGDPModelComparison %>%
    forecast(h = 20)

plotComparison <- autoplot(chineseGDP, level = NULL) +
    autolayer(chineseGDPModelComparisonForecast, series = "SimpleETS", PI = FALSE) +
    autolayer(chineseGDPModelComparisonForecast, series = "BoxCoxETS", PI = FALSE) +
    autolayer(chineseGDPModelComparisonForecast, series = "DampedETS", PI = FALSE) +
    autolayer(chineseGDPModelComparisonForecast, series = "LogETS", PI = FALSE) +
    labs(title = 'Chinese GDP ETS Model Comparison') +
    theme(legend.position = "bottom")

# print(plotComparison)