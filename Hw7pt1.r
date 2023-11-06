#6.3
library(caret)
library(dplyr)
library(RANN)
library(DataExplorer)
library(AppliedPredictiveModeling)

#a
data(ChemicalManufacturingProcess)
#print(summary(ChemicalManufacturingProcess))

#b
#filling in missing data using knnImpute
preProcess <- preProcess(ChemicalManufacturingProcess, method = c("knnImpute"))

imputation <- predict(preProcess, ChemicalManufacturingProcess)

#c
print("Imputation")
#print(head(imputation))

inTrain <- createDataPartition(imputation$Yield, p = 0.8, list = FALSE)
train <- imputation[inTrain, ]
test <- imputation[-inTrain, ]

pcrModel <- train(Yield ~ ., data = train, method = "pcr",
  center = TRUE,
  trControl = trainControl("cv", number = 10),
  tuneLength = 25
)

print("pcr Model")
print(pcrModel)
#optimal number of latent variables is 17
#0.686 for r2 score

#d
predictPcr <- predict(pcrModel, test)

check <- data.frame(obs = test$Yield, pred = predictPcr)
colnames(check) <- c("obs", "pred")
print("pcr prediction")
print(defaultSummary(check))

#prediction performed better than model
#0.5171 is lower than an RMSE of 0.6603
#e
important <- varImp(pcrModel)
print("list of which predictor is most")
print(important)

#f
correlations <- imputation %>%
  select(
        ManufacturingProcess32, ManufacturingProcess13, BiologicalMaterial06, ManufacturingProcess36,
        ManufacturingProcess17, ManufacturingProcess09, BiologicalMaterial03, BiologicalMaterial02,
        BiologicalMaterial12, ManufacturingProcess06, ManufacturingProcess31, ManufacturingProcess11,
        ManufacturingProcess33, BiologicalMaterial11, BiologicalMaterial08, BiologicalMaterial04
        )

plot_correlation(correlations)