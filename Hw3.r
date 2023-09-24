#5.1, 5.2, 5.3, 5.4 and 5.7

library(fpp3)
library(forecast)
library(fable)
library(tidyverse)
library(ggplot2)
library(lubridate) 

# glimpse(global_economy)
ausPop <- global_economy %>%
  filter(Country == "Australia") %>%
  select(Year, Population) %>%
  tsibble(index = Year)

ausPlot <- ausPop %>%
  ggplot(aes(x = Year, y = Population)) +
  geom_line() +
  labs(title = "Australian Population Over Time")

#print(ausPlot)
#thought naive fit best for this graph over 5 year span so we can see 
fitAusPop <- ausPop %>%
  model(
    NAIVE(Population)
  )

forecastAusPop <- fitAusPop %>%
  forecast(h = "5 years")

#print(forecastAusPop)
# the mean was the same so it is a good indicator that it was naive

#----------------------------------------------------------------
# glimpse(aus_production)
ausBricks <- aus_production %>%
  select(Quarter, Bricks) %>%
  filter(!is.na(Bricks)) %>%
  tsibble(index = Quarter)

bricksPlot <- ausBricks %>%
  ggplot(aes(x = Quarter, y = Bricks)) +
  geom_line() +
  labs(title = "Australian Bricks Production Over Time")

print(bricksPlot)

fitAusBricks <- ausBricks %>%
  model(
    RW(Bricks ~ drift())
  )
#12 months
forecastAusBricks <- fitAusBricks %>%
  forecast(h = 12)

print(forecastAusBricks)
#moves a lot of bump on the slope
#----------------------------------------------------------------
# glimpse(aus_livestock)

ausLambs <- aus_livestock %>%
  filter(Animal == "Lambs", State == "New South Wales") %>%
  select(Month, Count) %>%
  tsibble(index = Month)

lambsPlot <- ausLambs %>%
  ggplot(aes(x = Month, y = Count)) +
  geom_line() +
  labs(title = "NSW Lambs Over Time", 
       x = "Month", 
       y = "Count of Lambs")

#print(lambsPlot)

fitAusLambs <- ausLambs %>%
  model(
    RW(Count ~ drift())
  )
#12 months
forecastAusLambs <- fitAusLambs %>%
  forecast(h = 12)

#print(forecastAusLambs)
#losing lambs monthly starting at 2019 but also bumpy

#----------------------------------------------------------------
# glimpse(hh_budget)
hhWealth <- hh_budget %>%
  summarise(Wealth = mean(Wealth, na.rm = TRUE)) %>%
  as_tsibble(index = Year)

wealthPlot <- hhWealth %>%
  ggplot(aes(x = Year, y = Wealth)) +
  geom_line() +
  labs(title = "Household Wealth Over Time",
       x = "Year",
       y = "Wealth")

#print(wealthPlot)

fitHHWealth <- hhWealth %>%
  model(RW(Wealth ~ drift()))
#5 years
forecastHHWealth <- fitHHWealth %>%
  forecast(h = 5)

#print(forecastHHWealth)
#bumpy slope with no seasonal, but could be trend

#----------------------------------------------------------------
# glimpse(aus_retail)
#filter and sum data for takeaway
ausRetail <- as.data.frame(aus_retail)

takeawayFood <- ausRetail %>%
  filter(Industry == "Takeaway food services") %>%
  select(Month, Turnover) %>%
  group_by(Month) %>%
  summarize(TotalTurnover = sum(Turnover, na.rm = TRUE)) %>%
  ungroup()

takeawayPlot <- takeawayFood %>%
  ggplot(aes(x = Month, y = TotalTurnover)) +
  geom_line() +
  labs(title = "Australian Takeaway Food Turnover Over Time",
       x = "Month",
       y = "Total Turnover")

print(takeawayPlot)

fitTakeawayFood <- takeawayFood %>%
  tsibble::as_tsibble(index = Month) %>%
  model(
    RW(TotalTurnover ~ drift())
  )

#12 months
forecastTakeawayFood <- fitTakeawayFood %>%
  forecast(h = "12 months")

print(forecastTakeawayFood)
#bumpy with increase in the slope