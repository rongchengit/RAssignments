#2.4

#i really hate this as a means to downloading data. it is disgusting
# install.packages("USgas")

library(USgas)
library(tsibble)
library(dplyr)
library(ggplot2)

#year as the index and state as the key
gasTsibble <- us_total %>% as_tsibble(index = year, key = state)

print(gasTsibble)


#
newEnglandStates <- c("Maine", "Vermont", "New Hampshire", "Massachusetts", "Connecticut", "Rhode Island")

# Filter data for only New England States
newEnglandData <- gasTsibble %>%
  filter(state %in% newEnglandStates)

#year state and y assuming y is comsumption
print(names(newEnglandData)) 

englandPlot <- ggplot(newEnglandData, aes(x = year, y = y, color = state)) +
  geom_line() +
  labs(title = "Annual Natural Gas Consumption in New England",
       y = "Natural Gas Consumption", x = "Year") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

print(englandPlot)