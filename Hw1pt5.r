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

#----------------------------------------------------------------
bricksTsibble <- aus_production %>% select(Quarter, Bricks)
#print(bricksTsibble)

bricksPlot <- autoplot(bricksTsibble, Bricks) +
  labs(title = "Bricks from aus_production")
#print(bricksPlot)

bricksSeasonPlot <- gg_season(bricksTsibble, Bricks)
#print(bricksSeasonPlot)

bricksSeriesPlot <- gg_subseries(bricksTsibble, Bricks)
#print(bricksSeriesPlot)

bricksLagPlot <- gg_lag(bricksTsibble, Bricks)
#print(bricksLagPlot)

bricksAcfPlot <- ACF(bricksTsibble, Bricks)
#print(bricksAcfPlot)

#----------------------------------------------------------------

hareTsibble <- pelt %>% select(Year, Hare)
#print(hareTsibble)

harePlot <- autoplot(hareTsibble, Hare) +
  labs(title = "Hare from pelt")
#print(harePlot)

# # skip the gg_season plot due to annual data
# hareSeasonPlot <- gg_season(hareTsibble, Hare)
# #print(hareSeasonPlot)

hareSeriesPlot <- gg_subseries(hareTsibble, Hare)
#print(hareSeriesPlot)

hareLagPlot <- gg_lag(hareTsibble, Hare)
#print(hareLagPlot)

hareAcfPlot <- ACF(hareTsibble, Hare)
#print(hareAcfPlot)

#----------------------------------------------------------------
#print(?PBS)

# downgrading to format
# Convert tsibble to tibble and group by Month to sum the cost
waterCostTsibble <- PBS %>%
  as_tibble() %>%
  group_by(Month) %>%
  summarise(Cost = sum(Cost))

#reformating back to tsibble
waterCostTsibble <- as_tsibble(waterCostTsibble, index = Month)

# Auto plot
waterCostPlot <- autoplot(waterCostTsibble, Cost) +
  labs(title = " water cost from PBS")
#print(waterCostPlot)

# Auto plot
waterCostPlot <- autoplot(waterCostTsibble, Cost) +
  labs(title = " water cost from PBS")
#print(waterCostPlot)

# Seasonal plot
waterCostSeasonPlot <- gg_season(waterCostTsibble, Cost)
#print(waterCostSeasonPlot)

# Subseries plot
waterCostSeriesPlot <- gg_subseries(waterCostTsibble, Cost)
#print(waterCostSeriesPlot)

# Lag plot
waterCostLagPlot <- gg_lag(waterCostTsibble, Cost)
#print(waterCostLagPlot)

# ACF plot
waterCostAcfPlot <- ACF(waterCostTsibble, Cost)
#print(waterCostAcfPlot)
#----------------------------------------------------------------

# print(?us_gasoline)

barrelsTsibble <- us_gasoline %>% select(Week, Barrels)
print(barrelsTsibble)

barrelsPlot <- autoplot(barrelsTsibble, Barrels) +
  labs(title = "Barrels from us_gasoline")
print(barrelsPlot)

barrelsSeasonPlot <- gg_season(barrelsTsibble, Barrels)
#print(barrelsSeasonPlot)

barrelsSeriesPlot <- gg_subseries(barrelsTsibble, Barrels)
#print(barrelsSeriesPlot)

barrelsLagPlot <- gg_lag(barrelsTsibble, Barrels)
#print(barrelsLagPlot)

barrelsAcfPlot <- ACF(barrelsTsibble, Barrels)
print(barrelsAcfPlot)