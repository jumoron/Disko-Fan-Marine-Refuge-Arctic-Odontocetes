###REUSABLE CLASSIFIER SCRIPT
## 0. Packages ----
library(dplyr)
library(caret)
library(randomForest)

## 1. Prepare data: NW / NBW / SPW only ----
# Choose features (same set you’ve been using)
feat <- c("sampleDuration", "zeroCrossings", "RL_peak",
          "R_25_40", "TOL_25kHz", "TOL_40kHz",
          "X3db_center_frequency")

click_df <- tudo_up %>%
  filter(event_type %in% c("NW", "NBW", "SPW")) %>%
  select(event_type, all_of(feat)) %>%
  filter(complete.cases(.)) %>%
  mutate(event_type = factor(event_type))

# Quick check
summary(click_df$event_type)
str(click_df[, feat])

## 2. Train / test split ----
set.seed(123)
idx <- createDataPartition(click_df$event_type, p = 0.7, list = FALSE)
train_df <- click_df[idx, ]
test_df  <- click_df[-idx, ]

## 3. Train random forest classifier ----
set.seed(123)
rf_mod <- randomForest(
  event_type ~ .,
  data = train_df,
  ntree = 1000,
  mtry  = 3,           # you can tune this
  importance = TRUE,
  na.action = na.omit
)

print(rf_mod)
rf_mod$confusion[, "class.error"]   # per‑species OOB error
rf_mod$err.rate[nrow(rf_mod$err.rate), "OOB"]  # overall OOB error

varImpPlot(rf_mod, main = "Click feature importance")

## 4. Test‑set performance ----
pred_test <- predict(rf_mod, newdata = test_df)
cm <- caret::confusionMatrix(pred_test, test_df$event_type)
cm

## 5. Helper: classify new clicks ----
# new_clicks must have the same feature columns as `feat`
predict_click_species <- function(new_clicks, rf_model = rf_mod) {
  new_clicks %>%
    select(all_of(feat)) %>%
    mutate(across(everything(), as.numeric)) %>%
    { 
      probs <- predict(rf_model, newdata = ., type = "prob")
      class <- colnames(probs)[max.col(probs)]
      cbind(., 
            species_rf = class,
            max_prob   = apply(probs, 1, max))
    }
}

## Example:
# new_res <- predict_click_species(click_df[1:10, ])
# head(new_res)