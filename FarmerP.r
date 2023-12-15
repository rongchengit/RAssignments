#libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)
library(Metrics)
library(lubridate)
library(zoo)
library(glmnet)

#dataset
cropData <- read.csv("/Users/rongchen/Desktop/FruitAndVegetablePrices.csv")

#convert prices to numeric by removing the '$' sign and converting to numeric
cleanPriceColumn <- function(priceColumn) {
  cleanedPrices <- gsub(",", "", gsub("\\$", "", priceColumn))
  return(as.numeric(cleanedPrices))
}

priceColumns <- c("farmprice", "atlantaretail", "chicagoretail", "losangelesretail", "newyorkretail")
for (columnName in priceColumns) {
  cropData[[columnName]] <- cleanPriceColumn(cropData[[columnName]])
}

#adjusting the data format for predictions
cropData$date <- as.Date(cropData$date, format = "%Y-%m-%d")
cropData$month <- month(cropData$date)
cropData$year <- year(cropData$date)

#adjusting data for forecasting
futurePeriods <- data.frame(
    date = seq(max(cropData$date), by = "month", length.out = 12), #12 months
    farmprice = rep(mean(cropData$farmprice, na.rm = TRUE), 12), #average farm price
    month = month(seq(max(cropData$date), by = "month", length.out = 12)),
    year = year(seq(max(cropData$date), by = "month", length.out = 12))
)

#----------------------------------------------------------------
#visual for the dataset for farm prices
longData <- cropData %>%
  gather(key = "region", value = "price", atlantaretail, chicagoretail, losangelesretail, newyorkretail)

fplot <- ggplot() +
  geom_bar(data = longData, aes(x = productname, y = price, fill = region), stat = "identity", position = "dodge") +
  geom_line(data = cropData, aes(x = productname, y = farmprice, group = 1), color = "black", linewidth = 1) +
  theme_minimal() +
  labs(title = "Comparison of Farm Prices with Retail Prices in Different Regions",
       x = "Product Name",
       y = "Price (USD)",
       fill = "Region") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# print(fplot)


#----------------------------------------------------------------
#building models
#spliting training and testing
set.seed(123)
trainingData <- sample_frac(cropData, 0.8)
testingData <- setdiff(cropData, trainingData)
trainingData <- na.omit(trainingData)
testingData <- na.omit(testingData)

#linear regression
linearModel <- train(atlantaretail ~ farmprice + month + year, 
                     data = trainingData, 
                     method = "lm",
                     trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3))

print("Linear regression model")
print(linearModel)

#ridge regression
tuneGridRidge <- expand.grid(alpha = 0, lambda = 10^seq(-3, 3, length = 10))

ridgeModel <- train(atlantaretail ~ farmprice + month + year, 
                    data = trainingData, 
                    method = "glmnet", 
                    trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3),
                    tuneGrid = tuneGridRidge)

print("Ridge regression model")
print(ridgeModel)

#lasso regression
tuneGridLasso <- expand.grid(alpha = 1, lambda = 10^seq(-3, 3, length = 10))

lassoModel <- train(atlantaretail ~ farmprice + month + year, 
                    data = trainingData, 
                    method = "glmnet", 
                    trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3),
                    tuneGrid = tuneGridLasso)

print("Lasso regression model")
print(lassoModel)

#elastic regression
tuneGridElasticNet <- expand.grid(alpha = seq(0, 1, length = 5), lambda = 10^seq(-3, 3, length = 10))

elasticNetModel <- train(atlantaretail ~ farmprice + month + year, 
                         data = trainingData, 
                         method = "glmnet", 
                         trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3),
                         tuneGrid = tuneGridElasticNet)

print("Elastic Net regression model")
print(elasticNetModel)

#----------------------------------------------------------------
#evaluation of models
#linear regression
predictionsLinear <- predict(linearModel, newdata = testingData)
rmseLinear <- rmse(testingData$atlantaretail, predictionsLinear)
maeLinear <- mae(testingData$atlantaretail, predictionsLinear)

#ridge regression
predictionsRidge <- predict(ridgeModel, newdata = testingData)
rmseRidge <- rmse(testingData$atlantaretail, predictionsRidge)
maeRidge <- mae(testingData$atlantaretail, predictionsRidge)

#lasso regression
predictionsLasso <- predict(lassoModel, newdata = testingData)
rmseLasso <- rmse(testingData$atlantaretail, predictionsLasso)
maeLasso <- mae(testingData$atlantaretail, predictionsLasso)

#elastic
predictionsElasticNet <- predict(elasticNetModel, newdata = testingData)
rmseElasticNet <- rmse(testingData$atlantaretail, predictionsElasticNet)
maeElasticNet <- mae(testingData$atlantaretail, predictionsElasticNet)

#----------------------------------------------------------------
#selecting best model
modelEvaluations <- data.frame(
  Model = c("Linear", "Ridge", "Lasso", "ElasticNet"),
  RMSE = c(rmseLinear, rmseRidge, rmseLasso, rmseElasticNet),
  MAE = c(maeLinear, maeRidge, maeLasso, maeElasticNet)
)

#lowest rmse
bestModelRMSE <- modelEvaluations[which.min(modelEvaluations$RMSE), "Model"]

#lowest mae
bestModelMAE <- modelEvaluations[which.min(modelEvaluations$MAE), "Model"]
#----------------------------------------------------------------
#printing 
cat("Linear Regression Model Evaluation:\n")
cat("RMSE:", rmseLinear, "\n")
cat("MAE:", maeLinear, "\n\n")

cat("Ridge Regression Model Evaluation:\n")
cat("RMSE:", rmseRidge, "\n")
cat("MAE:", maeRidge, "\n\n")

cat("Lasso Regression Model Evaluation:\n")
cat("RMSE:", rmseLasso, "\n")
cat("MAE:", maeLasso, "\n\n")

cat("Elastic Net Regression Model Evaluation:\n")
cat("RMSE:", rmseElasticNet, "\n")
cat("MAE:", maeElasticNet, "\n\n")

cat("Best model based on RMSE:", bestModelRMSE, "\n")
cat("Best model based on MAE:", bestModelMAE, "\n")

#----------------------------------------------------------------
#forecast and predictions

#loop through each region for predictions and forecasts
regions <- c("atlantaretail", "chicagoretail", "losangelesretail", "newyorkretail")
allPredictions <- list()
allForecasts <- list()

for(region in regions) {
    #training
    formula <- as.formula(paste(region, "~ farmprice + month + year"))
    model <- lm(formula, data = trainingData)
    
    #predictions
    predictions <- predict(model, newdata = testingData)
    allPredictions[[region]] <- data.frame(Date = testingData$date, 
                                           ActualPrice = testingData[[region]], 
                                           PredictedPrice = predictions)

    #forecasting
    forecast <- predict(model, newdata = futurePeriods)
    allForecasts[[region]] <- data.frame(Date = futurePeriods$date, ForecastedPrice = forecast)
}

#print predictions and forecasts for each region
for(region in regions) {
    cat(paste("Predictions for", region, ":\n"))
    print(head(allPredictions[[region]]))
    cat(paste("Forecast for", region, ":\n"))
    print(head(allForecasts[[region]]))
}

#----------------------------------------------------------------
#visual graphs for predictions and forecasts
atlantaPredictionData <- allPredictions[["atlantaretail"]] %>% 
    mutate(ForecastedPrice = NA, Type = "Actual/Predicted")
atlantaForecastData <- allForecasts[["atlantaretail"]] %>% 
    mutate(ActualPrice = NA, PredictedPrice = NA, Type = "Forecasted")
atlantaCombinedData <- rbind(atlantaPredictionData, atlantaForecastData)

atlantaPlot <- ggplot(atlantaCombinedData, aes(x = Date)) +
    geom_line(aes(y = ActualPrice, color = "Actual Price"), data = atlantaCombinedData %>% filter(Type == "Actual/Predicted")) +
    geom_line(aes(y = PredictedPrice, color = "Predicted Price"), data = atlantaCombinedData %>% filter(Type == "Actual/Predicted")) +
    geom_line(aes(y = ForecastedPrice, color = "Forecasted Price"), data = atlantaCombinedData %>% filter(Type == "Forecasted")) +
    labs(title = "Actual, Predicted, and Forecasted Prices for Atlanta",
         x = "Date", y = "Price (USD)") +
    scale_color_manual(values = c("Actual Price" = "blue", "Predicted Price" = "red", "Forecasted Price" = "green")) +
    theme_minimal()

# print(atlantaPlot)

chicagoPredictionData <- allPredictions[["chicagoretail"]] %>% 
    mutate(ForecastedPrice = NA, Type = "Actual/Predicted")
chicagoForecastData <- allForecasts[["chicagoretail"]] %>% 
    mutate(ActualPrice = NA, PredictedPrice = NA, Type = "Forecasted")
chicagoCombinedData <- rbind(chicagoPredictionData, chicagoForecastData)

chicagoPlot <- ggplot(chicagoCombinedData, aes(x = Date)) +
    geom_line(aes(y = ActualPrice, color = "Actual Price"), data = chicagoCombinedData %>% filter(Type == "Actual/Predicted")) +
    geom_line(aes(y = PredictedPrice, color = "Predicted Price"), data = chicagoCombinedData %>% filter(Type == "Actual/Predicted")) +
    geom_line(aes(y = ForecastedPrice, color = "Forecasted Price"), data = chicagoCombinedData %>% filter(Type == "Forecasted")) +
    labs(title = "Actual, Predicted, and Forecasted Prices for Chicago",
         x = "Date", y = "Price (USD)") +
    scale_color_manual(values = c("Actual Price" = "blue", "Predicted Price" = "red", "Forecasted Price" = "green")) +
    theme_minimal()

# print(chicagoPlot)

laPredictionData <- allPredictions[["losangelesretail"]] %>% 
    mutate(ForecastedPrice = NA, Type = "Actual/Predicted")
laForecastData <- allForecasts[["losangelesretail"]] %>% 
    mutate(ActualPrice = NA, PredictedPrice = NA, Type = "Forecasted")
laCombinedData <- rbind(laPredictionData, laForecastData)

laPlot <- ggplot(laCombinedData, aes(x = Date)) +
    geom_line(aes(y = ActualPrice, color = "Actual Price"), data = laCombinedData %>% filter(Type == "Actual/Predicted")) +
    geom_line(aes(y = PredictedPrice, color = "Predicted Price"), data = laCombinedData %>% filter(Type == "Actual/Predicted")) +
    geom_line(aes(y = ForecastedPrice, color = "Forecasted Price"), data = laCombinedData %>% filter(Type == "Forecasted")) +
    labs(title = "Actual, Predicted, and Forecasted Prices for Los Angeles",
         x = "Date", y = "Price (USD)") +
    scale_color_manual(values = c("Actual Price" = "blue", "Predicted Price" = "red", "Forecasted Price" = "green")) +
    theme_minimal()

# print(laPlot)

nyPredictionData <- allPredictions[["newyorkretail"]] %>% 
    mutate(ForecastedPrice = NA, Type = "Actual/Predicted")
nyForecastData <- allForecasts[["newyorkretail"]] %>% 
    mutate(ActualPrice = NA, PredictedPrice = NA, Type = "Forecasted")
nyCombinedData <- rbind(nyPredictionData, nyForecastData)

nyPlot <- ggplot(nyCombinedData, aes(x = Date)) +
    geom_line(aes(y = ActualPrice, color = "Actual Price"), data = nyCombinedData %>% filter(Type == "Actual/Predicted")) +
    geom_line(aes(y = PredictedPrice, color = "Predicted Price"), data = nyCombinedData %>% filter(Type == "Actual/Predicted")) +
    geom_line(aes(y = ForecastedPrice, color = "Forecasted Price"), data = nyCombinedData %>% filter(Type == "Forecasted")) +
    labs(title = "Actual, Predicted, and Forecasted Prices for New York",
         x = "Date", y = "Price (USD)") +
    scale_color_manual(values = c("Actual Price" = "blue", "Predicted Price" = "red", "Forecasted Price" = "green")) +
    theme_minimal()

# print(nyPlot)