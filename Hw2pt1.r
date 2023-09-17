
library(fpp3)
library(dplyr)
library(ggplot2)
library(fabletools)
library(forecast)

# glimpse(global_economy)

# usGDP <- global_economy %>%
#   filter(Country == "United States") %>%
#   select(Year, GDP)

# usGDPPlot <- ggplot(usGDP, aes(x = Year, y = GDP)) +
#   geom_line() +
#   labs(title = "United States GDP Over Time")

# print(usGDPPlot)

#opted not to transform since it seemed stable 
#i might have used a log transformation if you wanted it to see the rate of growth
#----------------------------------------------------------------

# glimpse(aus_livestock)

# vicSlaughter <- aus_livestock %>%
#   filter(State == "Victoria" & Animal == "Bulls, bullocks and steers") %>%
#   select(Month, Count)

# # vicSlaughterPlot <- ggplot(vicSlaughter, aes(x = Month, y = Count)) +
# #   geom_line() +
# #   labs(title = "Slaughter of Victorian 'Bulls, bullocks and steers'")

# # print(vicSlaughterPlot)

# vicTs <- ts(vicSlaughter$Count, frequency = 12)  #12 is for months

# #decompose the time series
# decomp <- decompose(vicTs)
# plot(decomp)

#----------------------------------------------------------------

# glimpse(vic_elec)

# vicElecPlot <- ggplot(vic_elec, aes(x = Time, y = Demand)) +
#   geom_line() +
#   labs(title = "Victorian Electricity Demand Over Time",
#        x = "Time",
#        y = "Electricity Demand")

# print(vicElecPlot)
#i tried to find a seasonal transformation for electric but it was difficult to read
#im sure if i spend more time on it i could have nailed it, it was blurry and too much fluxation
#and i feel it is already consistent 
#maybe i'll try box-cox since it is somewhat stable
#----------------------------------------------------------------

# glimpse(aus_production)

# gasProductionPlot <- ggplot(aus_production, aes(x = Quarter, y = Gas)) +
#   geom_line() +
#   labs(title = "Gas Production Over Time in Australia",
#        x = "Time",
#        y = "Gas Production")

# print(gasProductionPlot)

# # #----------------------------------------------------------------
# ausProductionLog <- aus_production %>%
#   mutate(LogGas = log(Gas))

# #transforming data
# logPlot <- ggplot(ausProductionLog, aes(x = Quarter, y = LogGas)) +
#   geom_line() +
#   labs(title = "log gas production over time",
#        x = "Time", y = "Log(Gas Production)") +
#   theme_minimal()

# print(logPlot)

# #we can see the rate in which the gas production is changing exponentially

#----------------------------------------------------------------
# glimpse(canadian_gas)

# canadianGasPlot <- ggplot(canadian_gas, aes(x = Month, y = Volume)) +
#   geom_line() +
#   labs(title = "Canadian Gas Volume Over Time",
#        x = "Time", y = "Gas Volume") +
#   theme_minimal()

# print(canadianGasPlot)
# #because every 10 years it has a pretty consistent rate in which it moves
# #it has a negative rates
# #it is for stablized rates instead of changing variants
#----------------------------------------------------------------
#3.4
# #determine the best lambda value
# bestLambda <- BoxCox.lambda(aus_retail$Turnover)

# #applying box-cox
# ausRetailTransformed <- BoxCox(aus_retail$Turnover, lambda = bestLambda)

# print(paste("box cox determinator:", bestLambda))
#really close to 0 so we use a natural log just like aus_production, but my graph looked different so i got rid of it
#i used a subset as well, by i decided against it

#3.5
# glimpse(ansett)
# glimpse(pedestrian)

lambdaTobacco <- BoxCox.lambda(aus_production$Tobacco)
print(paste("Lambda for Tobacco:", lambdaTobacco))
#0.71, suggesting we use a power transformation close to the square root.

# Filtering the dataset for Melbourne-Sydney and Economy class 
subsetAnsett <- ansett[ansett$Airports == "MEL-SYD" & ansett$Class == "Economy" & ansett$Passengers > 0, ]
#because there was negative numbers in it, so we couldnt use that for box-cox
# Now get the lambda value for the filtered data
lambdaAnsett <- BoxCox.lambda(subsetAnsett$Passengers)
print(paste("Lambda for Ansett Economy Passengers between Melbourne and Sydney:", lambdaAnsett))
#Lambda is near 2. This means we should square the values.

# Filter the dataset for Southern Cross Station and ensure counts are positive
subsetPedestrian <- pedestrian[pedestrian$Sensor == "Southern Cross Station" & pedestrian$Count > 0, ]

# Now get the lambda value for the filtered data
lambdaPedestrian <- BoxCox.lambda(subsetPedestrian$Count)
print(paste("Lambda for Pedestrian Counts at Southern Cross Station:", lambdaPedestrian))
#Lambda is about 0.06, indicating a logarithmic transformation would be best.

#----------------------------------------------------------------