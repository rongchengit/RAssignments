
library(fpp3)
library(dplyr)
library(ggplot2)
library(fabletools)
library(forecast)

#plot timeseries
gas <- aus_production %>%
  tail(5*4) %>%
  select(Gas)

gasPlot <- ggplot(gas, aes(x = 1:nrow(gas), y = Gas)) + 
  geom_line() + 
  labs(title = "Last 5 years of Gas Data", x = "Time", y = "Gas Production") + 
  theme_minimal()

# print(gasPlot)

#its a trend
#every 2.5 years there is a high peak, followed by a low peak and they increment the peak as the years go by


gasTs <- ts(gas$Gas, frequency=4)  # Assuming quarterly data
decomposedGas <- decompose(gasTs, type="multiplicative")
# plot(decomposedGas)

# #yes they do,
#--------------------------------

tsGas <- ts(gas$Gas, frequency=4)
stlGas <- stl(tsGas, s.window = "periodic")
gas$seasonallyAdjusted <- tsGas / stlGas$time.series[,"seasonal"]

#plot orignal
originalPlot <- ggplot(gas, aes(x = 1:nrow(gas), y = seasonallyAdjusted)) + 
  geom_line(color = "blue") + 
  labs(title = "Original Seasonally Adjusted Gas Data", x = "Time", y = "Gas Production") + 
  theme_minimal()

#adding outlier in the middle
gasMiddleOutlier <- gas
gasMiddleOutlier$Gas[nrow(gas) %/% 2] <- gas$Gas[nrow(gas) %/% 2] + 300
tsGasMiddleOutlier <- ts(gasMiddleOutlier$Gas, frequency=4)
stlGasMiddleOutlier <- stl(tsGasMiddleOutlier, s.window = "periodic")
gasMiddleOutlier$seasonallyAdjusted <- tsGasMiddleOutlier / stlGasMiddleOutlier$time.series[,"seasonal"]

#plot adjusted for the middle outlier
middleOutlierPlot <- ggplot(gasMiddleOutlier, aes(x = 1:nrow(gas), y = seasonallyAdjusted)) + 
  geom_line(color = "red") + 
  labs(title = "Seasonally Adjusted with Middle Outlier", x = "Time", y = "Gas Production") + 
  theme_minimal()

#added outlier at the end
gasEndOutlier <- gas
gasEndOutlier$Gas[nrow(gas) - 1] <- gas$Gas[nrow(gas) - 1] + 300
tsGasEndOutlier <- ts(gasEndOutlier$Gas, frequency=4)
stlGasEndOutlier <- stl(tsGasEndOutlier, s.window = "periodic")
gasEndOutlier$seasonallyAdjusted <- tsGasEndOutlier / stlGasEndOutlier$time.series[,"seasonal"]

#plot adjusted for end outlier
endOutlierPlot <- ggplot(gasEndOutlier, aes(x = 1:nrow(gas), y = seasonallyAdjusted)) + 
  geom_line(color = "purple") + 
  labs(title = "Seasonally Adjusted with End Outlier", x = "Time", y = "Gas Production") + 
  theme_minimal()

# Now, you can print the plots to observe:
#print(originalPlot)
#print(middleOutlierPlot)
print(endOutlierPlot)