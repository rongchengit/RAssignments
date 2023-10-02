
library(mlbench)
library(tidyr)
library(ggplot2)
library(corrplot)
library(GGally)
library(caret)
library(dplyr)
library(naniar)
library(recipes)
library(rpart)
library(rpart.plot)
library(missForest)


suppressWarnings({

    # #load data
    # data(Soybean)
    # str(Soybean)

    soybeanLong <- Soybean %>% 
    select_if(is.factor) %>% 
    gather(key = "predictor", value = "value")

    frequencyDistPlot <- ggplot(soybeanLong, aes(x=value)) +
    geom_bar(aes(fill=value), position="dodge") +
    facet_wrap(~ predictor, scales = "free_x", ncol = 5) + 
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Frequency Distribution of Categorical Predictors in Soybean Dataset")

    # print(frequencyDistPlot)    

    missingVarPlot <- gg_miss_var(Soybean, show_pct = TRUE) +
    labs(title = "Percentage of Missing Data by Predictor") +
    theme_minimal()

    # print(missingVarPlot)

    # missingSummary <- data.frame(
    # Metric = c("Number of Missing Values", 
    #             "Number of Complete Values",
    #             "Proportion of Missing Values",
    #             "Proportion of Complete Values",
    #             "Percentage of Missing Values",
    #             "Percentage of Complete Values"),
    # Value = c(n_miss(Soybean),
    #             n_complete(Soybean),
    #             round(prop_miss(Soybean), 4),
    #             round(prop_complete(Soybean), 4),
    #             round(pct_miss(Soybean), 2),
    #             round(pct_complete(Soybean), 2))
    # )
    # # print("Summary of Missing Data Metrics:")
    # # print(missingSummary)

    # missingnessByClass <- Soybean %>%
    # group_by(Class) %>%
    # summarise_all(list(missing_pct = ~mean(is.na(.)) * 100))

    # # print("Missing Percentage by Class:")
    # # print(missingnessByClass)

# #graph for missing----------------------------------------------------------------
#   print("Missing Data Summary Grouped by Class:")
#   missingnessByClass <- Soybean %>%
#   group_by(Class) %>%
#   summarise_all(list(missing_pct = ~mean(is.na(.)) * 100))

#   Soybean %>%
#   group_by(Class) %>%
#   miss_var_summary() %>%
#   arrange(desc(pct_miss)) %>%
#   print(n=50)

#   #decision Tree to Understand Patterns of Missing Data#
#   treeModel <- Soybean %>%
#   add_prop_miss() %>%
#   rpart(prop_miss_all ~ ., data = .)

#   prp(treeModel, type = 4, extra = 101, prefix = "Prop. Miss = ", roundint = FALSE)

    #strategy for handling missing data, either by eliminating predictors or imputation
    #missing data list of columns with missing data
    gapCols <- colnames(Soybean)[apply(Soybean, 2, function(x) any(is.na(x)))]

    print("Columns with gaps:")
    print(gapCols)

    #class is not included with filling
    #using missForest to fill in gaps
    print("Filling gaps using missForest...")
    filledData <- missForest(select(Soybean, -Class), ntree = 1000, verbose = TRUE)

    #merge class and filled data together
    SoybeanWithFilledData <- bind_cols(Class = Soybean$Class, filledData$ximp)

    print("merged")

    #fill accuracy 
    print(paste("Fill accuracy:", filledData$OOBerror))
})