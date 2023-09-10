#2.8

library(fpp3)
library(dplyr)
library(ggplot2)
library(fabletools)
library(feasts)

totalPrivateEmployed <- us_employment %>% select(Month, `Total Private`)

# Basic plot
autoplot(totalPrivateEmployed, `Total Private`) +
  labs(title = "Total Private Employed from us_employment")

# Seasonal plot
gg_season(totalPrivateEmployed, `Total Private`)

# Subseries plot
gg_subseries(totalPrivateEmployed, `Total Private`)

# Lag plot
gg_lag(totalPrivateEmployed, `Total Private`)

# ACF plot
ACF(totalPrivateEmployed, `Total Private`)