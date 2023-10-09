#8.8 8.9
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

set.seed(1234567)
    myseries <- aus_retail %>%
    filter(`Series ID` == sample(aus_retail$`Series ID`, 1))

    myseries %>% autoplot(Turnover)
    print(myseries)

#the seasonal changes in this series get bigger as time goes on, so we need to use multiplicative seasonality

fit <- myseries %>%
    model(
        'Holt Winters Multiplicative Method' = ETS(Turnover ~ error('M') + trend('A') + season('M')),
        'Holt Winters Damped Method' = ETS(Turnover ~ error('M') + trend('Ad') + season('M'))
    )

HoltWinters <- fit %>% forecast(h = 10)

plotHolt <- HoltWinters %>% autoplot(myseries, level = NULL)
print(plotHolt)

# #can't do because my accuracy function doesnt work
# accuracy(fit) %>% select('.model', 'RMSE')