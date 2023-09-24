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

recent_production <- aus_production %>% 
  filter(year(Quarter) >= 1992)

print(ggplot(recent_production, aes(x = Quarter, y = Beer)) + geom_line())

fit <- recent_production %>% 
  model(SNAIVE(Beer))

print(fit)

#residuals
gg_tsresiduals(fit)

forecasts <- fit %>% forecast()
combined_data <- bind_rows(
  recent_production %>% mutate(Type = "Actual", Beer = Beer),
  forecasts %>% as_tibble() %>% select(Quarter, .mean) %>%
    rename(Beer = .mean) %>% mutate(Type = "Forecast")
)

resProd <- ggplot(combined_data, aes(x = Quarter, y = Beer, color = Type)) + 
  geom_line() + 
  labs(title = "Actual vs Forecasted Beer Production",
       y = "Beer Production")

print(resProd)