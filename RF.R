library(randomForest)
library(caret)

# Prep data (replace sampleDuration with actual name if needed)
rf_vars <- c("sampleDuration", "zeroCrossings", 
             "RL_peak", "TOL_25kHz", "TOL_40kHz")

rf_df <- tudo %>%
  select(all_of(rf_vars), event_type) %>%
  filter(complete.cases(.)) %>%
  mutate(event_type = factor(event_type))

# Split train/test
set.seed(123)
train_idx <- createDataPartition(rf_df$event_type, p = 0.7, list = FALSE)
train_df <- rf_df[train_idx, ]
test_df <- rf_df[-train_idx, ]

# Train RF
rf_mod <- randomForest(
  event_type ~ ., 
  data = train_df,
  ntree = 1000,
  importance = TRUE,
  na.action = na.omit
)

# Results
print(rf_mod)
rf_mod$confusion[, "class.error"]  # Per-class error
rf_mod$err.rate[nrow(rf_mod$err.rate), "OOB"]  # Final OOB error

##Variable Importance Plot
varImpPlot(rf_mod, main = "Click Feature Importance")
importance(rf_mod, type = 1)  # MeanDecreaseAccuracy

###test predictions
pred_test <- predict(rf_mod, test_df)
confusionMatrix(pred_test, test_df$event_type)