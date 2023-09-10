#2.8

library(fpp3)
library(dplyr)
library(ggplot2)
library(fabletools)
library(feasts)

#print(?us_employment)

# Filter data for "Total Private"
totalPrivateEmployed <- us_employment %>% 
  filter(Title == "Total Private") %>%
  select(Month, Employed)

# Auto plot
employPlot <- autoplot(totalPrivateEmployed, Employed) +
  labs(title = "Total Private Employed from us_employment")

# Seasonal plot
seasonPlot <- gg_season(totalPrivateEmployed, `Employed`)

# Subseries plot
seriesPlot <- gg_subseries(totalPrivateEmployed, `Employed`)

# Lag plot
lagPlot <- gg_lag(totalPrivateEmployed, `Employed`)

# ACF plot
acfPlot <- ACF(totalPrivateEmployed, `Employed`)

#print(employPlot)
#print(seasonPlot)
#print(seriesPlot)
#print(lagPlot)
#print(acfPlot)