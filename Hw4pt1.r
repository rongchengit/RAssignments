
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

suppressWarnings({

    data(Soybean)
    str(Soybean)

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

    print("Plot of Missing Values by Predictor:")
    # print(missingVarPlot)

    missingSummary <- data.frame(
    Metric = c("Number of Missing Values", 
                "Number of Complete Values",
                "Proportion of Missing Values",
                "Proportion of Complete Values",
                "Percentage of Missing Values",
                "Percentage of Complete Values"),
    Value = c(n_miss(Soybean),
                n_complete(Soybean),
                round(prop_miss(Soybean), 4),
                round(prop_complete(Soybean), 4),
                round(pct_miss(Soybean), 2),
                round(pct_complete(Soybean), 2))
    )
    print("Summary of Missing Data Metrics:")
    # print(missingSummary)

    missingnessByClass <- Soybean %>%
    group_by(Class) %>%
    summarise_all(list(missing_pct = ~mean(is.na(.)) * 100))

    print("Missingness Percentage by Class:")
    # print(missingnessByClass)

#strat----------------------------------------------------------------
  print("Missing Data Summary Grouped by Class:")
  missingnessByClass <- Soybean %>%
  group_by(Class) %>%
  summarise_all(list(missing_pct = ~mean(is.na(.)) * 100))

  Soybean %>%
  group_by(Class) %>%
  miss_var_summary() %>%
  arrange(desc(pct_miss)) %>%
  print(n=50)

  print("Decision Tree to Understand Patterns of Missing Data:")
  treeModel <- Soybean %>%
  add_prop_miss() %>%
  rpart(prop_miss_all ~ ., data = .)

  prp(treeModel, type = 4, extra = 101, prefix = "Prop. Miss = ", roundint = FALSE)

})