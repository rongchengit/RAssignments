# install.packages("nnet")
library(AppliedPredictiveModeling)
library(caret)
library(nnet)
library(ggplot2)
library(lattice)

data(ChemicalManufacturingProcess)

imputed_data <- preProcess(ChemicalManufacturingProcess, "knnImpute")
full_data <- predict(imputed_data, ChemicalManufacturingProcess)

low_values <- nearZeroVar(full_data)
chem_data <- full_data[,-low_values]

index_chem <- createDataPartition(chem_data$Yield , p=.8, list=F)

train_chem <-  chem_data[index_chem,] 
test_chem <- chem_data[-index_chem,]

#-knn-  
knnModel <- train(Yield~., 
                    data = train_chem,
                    method = "knn",
                    preProc = c("center", "scale"), 
                    tuneLength = 10)

knnPred <- predict(knnModel,  test_chem)
print("knn")
print(postResample(pred = knnPred, obs = test_chem$Yield))

#-tuned neural network
nnetGrid <- expand.grid(.decay = c(0, 0.01, .1),
                        .size = c(1:10), .bag = FALSE)

nnetTune <- train(Yield~., 
                  data = train_chem, 
                  method = "avNNet", 
                  tuneGrid = nnetGrid,
                  trControl = trainControl(method = "cv"), 
                  linout = TRUE,trace = FALSE,
                  MaxNWts = 10 * (ncol(train_chem) + 1) + 10 + 1, 
                  maxit = 500)

nnetPred <- predict(nnetTune,  test_chem)
print("nnet")
print(postResample(predict(nnetTune,  test_chem), test_chem$Yield))

#-mars tuned
marsTuned_chem <- train(Yield~. ,
                  data = train_chem,
                   method = "earth",
                   tuneGrid = marsGrid,
                   trControl = trainControl(method = "cv"))

marsTunePred_chem <- predict(marsTuned_chem,  test_chem)
print("mars")
print(postResample(marsTunePred_chem, test_chem$Yield))

#-svm tuned
svmTuned_chem <- train(Yield~. ,
                  data = train_chem,
                   method = "svmRadial",
                   tuneLength = 15,
                   trControl = trainControl(method = "cv"))

svmTunePred_chem <- predict(svmTuned_chem,  test_chem)
print("svm")
print(postResample(svmTunePred_chem, test_chem$Yield))

#b
print("most important in the optimal nonlinear regression model")
print(plot(varImp(nnetTune), top=10))

#c
corr_vals <- chem_data %>% 
  dplyr::select('Yield', 'ManufacturingProcess32','ManufacturingProcess36',
         'BiologicalMaterial06','ManufacturingProcess13',
         'BiologicalMaterial03','ManufacturingProcess17',
         'BiologicalMaterial02','BiologicalMaterial12',
         'ManufacturingProcess09','ManufacturingProcess31')

corr_plot_vals <- cor(corr_vals)

hmPlot <- corrplot.mixed(corr_plot_vals, tl.col = 'black', tl.pos = 'lt', 
         upper = "number", lower="circle")
print(hmPlot)