
library(fpp3)
library(dplyr)
library(ggplot2)
library(fabletools)
library(forecast)
library(feasts)
library(seasonal)


# glimpse(aus_retail)

set.seed(12345678)
myseries <- aus_retail |>
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))

sum(is.na(myseries$Turnover))
sum(is.na(myseries$Month))

turnoverPlot <- ggplot(myseries, aes(x = Month, y = Turnover)) + 
  geom_line() + 
  labs(title = "Selected Series Turnover from aus_retail")

print(turnoverPlot)

# Convert the series into a ts object
turnoverTs <- ts(myseries$Turnover, start=c(year(min(myseries$Month)), month(min(myseries$Month))), frequency=12)

# Apply the X-11 decomposition
decomposedTurnover <- seas(turnoverTs)

# Print the decomposed object
print(decomposedTurnover)

# Plot the decomposed series
plot(decomposedTurnover)