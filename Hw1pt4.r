#2.5
#install.packages("readxl")

library(tsibble)
library(readxl)
library(dplyr)
library(ggplot2)

data <- readxl::read_excel("/Users/rongchen/Desktop/tourism.xlsx")

#print(data)

#tsibble conversion
tourismaTsibble <- tourism

#print(tourismaTsibble)

maxTripAverage <- tourism %>%
  group_by(Region, Purpose) %>% 
  summarise(avg_trips = mean(Trips, na.rm = TRUE)) %>% #calculates the average of trips for region and purpose and na.rm removes NA
  arrange(desc(avg_trips)) %>% #arrange from highest to lowest
  slice(1)

print(maxTripAverage)

tripByState <- tourism %>%
  group_by(State) %>%
  summarise(Total_Trips = sum(Trips, na.rm = TRUE)) %>% #calculates the total number of trips across everything
  as_tsibble()

print(tripByState)
