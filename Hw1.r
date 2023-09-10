#Rong Chen
#HW 1
#2.1, 2.2, 2.3, 2.4, 2.5 and 2.8
#Im using print because im using Visual Studio and basically wont stop them if I dont
#The code is ugly and inefficient because I dont know R, but it all works

#Libraries I used
#2.1
#install fpp3
#install.packages("tsibble")
#install.packages("fabletools")
#install.packages("ggplot2")
library(fpp3)
library(dplyr)
library(ggplot2)
library(fabletools)

#2.1
#shows the data on the side of the screen
# print(?aus_production) #uses Quarter
# print(?pelt) #uses Year
# print(?vic_elec) #uses Time
# print(?gafa_stock) #uses Date

#shows the data on the terminal which i prefer
# print(head(aus_production))
# print(names(aus_production))


#these datasets are already tsibbles so i dont need the tsibble()
#took me a while to figure that out
# print(bricksTsibble <- aus_production %>% select(Quarter, Bricks))
# print(lynxTsibble <- pelt %>% select(Year, Lynx))
# print(demandTsibble <- vic_elec %>% select(Time, Demand))
# print(closeTsibble <- gafa_stock %>% select(Date, Close))


# autoplotting them
# bricksPlot <- autoplot(bricksTsibble, Bricks)
# lynxPlot <- autoplot(lynxTsibble, Lynx)
# demandPlot <- autoplot(demandTsibble, Demand)
closePlot <- autoplot(closeTsibble, Close) +
  labs(
    title = "GAFA Stock Close Prices",
    x = "Time",
    y = "Close Price"
  )

# printing autoplots
# print(bricksPlot)
# print(lynxPlot)
# print(demandPlot)
# print(closePlot)

#2.2
# needed to find the 4 stocks
uniqueStocks <- unique(gafa_stock$Symbol)
print(uniqueStocks)

#Symbol is used to find the stocks
#groupby is used to put all the stocks in their group
#find the max for closing prices
#then ungroup them
#peakClosingPrices is used to hold everything together
peakClosingPrices <- gafa_stock %>%
  group_by(Symbol) %>%
  filter(Close == max(Close)) %>%
  ungroup()

print(peakClosingPrices)

