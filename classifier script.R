###REUSABLE CLASSIFIER SCRIPT
## 0. Packages ----
library(dplyr)
library(caret)
library(randomForest)

tudo_up <- read.csv("E:/2020-2021/NAR02-02_SM2M/all csv/tudo_up.csv",
                    stringsAsFactors = FALSE)  # or TRUE if you prefer

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


####new test data
library(dplyr)
library(readr)

# 1. Read new data
all_new <- read_csv("E:/2020-2021/NAR02-02_SM2M/all csv/all.csv")

# 2. Keep a copy of original columns
all_new_raw <- all_new

# 3. Check required predictors
feat
setdiff(feat, names(all_new)) ##ok

all_pred <- predict_click_species(
  newdata = all_new,
  rf_mod  = rf_mod,
  feat    = feat
)

#redefine predict-species
predict_click_species <- function(newdata, rf_mod, feat) {
  
  # ensure we only use the predictor columns
  X <- newdata[, feat, drop = FALSE]
  
  # class probabilities from RF
  prob <- predict(rf_mod, X, type = "prob")  # classification RF [web:357][web:362]
  
  # get predicted class and its probability
  species <- colnames(prob)[max.col(prob, ties.method = "first")]
  max_prob <- prob[cbind(seq_len(nrow(prob)),
                         max.col(prob, ties.method = "first"))]
  
  # return original data + probs + labels
  dplyr::bind_cols(
    newdata,
    as.data.frame(prob),
    tibble::tibble(
      species_rf = species,
      max_prob   = max_prob
    )
  )
}

#Call it on your prepared data
pred_out <- predict_click_species(
  newdata = pred_input,
  rf_mod  = rf_mod,
  feat    = feat
)

#Then merge back to the full table as before:
all_pred <- all_new %>%
  dplyr::left_join(
    pred_out %>% dplyr::select(row_id, species_rf, max_prob),
    by = "row_id"
  )

## look at the predicted table, just the first rows
head(pred_out)

# Or, full object in View pane (RStudio)
View(pred_out)

##check how many clicks per predicted class
table(pred_out$species_rf, useNA = "ifany")

#check confidence distribution
summary(pred_out$max_prob)
hist(pred_out$max_prob, breaks = 20)

##3. Sanity‑check on your new data with event_type
with(all_pred, table(event_type, species_rf, useNA = "ifany"))

### Evaluate on all.csv (independent test)
# Predict classes on test set
test_prob <- predict(rf_mod, all_df[, feat, drop = FALSE], type = "prob") 
test_pred <- colnames(test_prob)[max.col(test_prob, ties.method = "first")]

# Confusion matrix and accuracy
cm <- confusionMatrix(
  factor(test_pred, levels = levels(all_df$event_type)),
  all_df$event_type
)
cm

###max_prob and subset
max_prob <- test_prob[cbind(seq_len(nrow(test_prob)),
                            max.col(test_prob, ties.method = "first"))]

core_idx  <- max_prob >= 0.8        # stricter cutoff
core_pred <- test_pred[core_idx]
core_true <- all_df$event_type[core_idx]

length(core_true) / nrow(all_df)    # fraction kept
table(core_pred, core_true)

##confusion matrix on the core set
cm_core <- confusionMatrix(
  factor(core_pred, levels = levels(all_df$event_type)),
  core_true
)
cm_core

###Build a core table with predictions
core_df <- all_df[core_idx, ] %>%
  mutate(
    species_rf = test_pred[core_idx],
    max_prob   = max_prob[core_idx]
  )

##per‑class probabilities in the file:
prob_core <- as.data.frame(test_prob[core_idx, ])
core_df <- bind_cols(core_df, prob_core)

write_csv(core_df,"E:/2020-2021/NAR02-02_SM2M/all csv/all_core_maxprob0.8.csv")

-----------
## Example:
# new_res <- predict_click_species(click_df[1:10, ])
# head(new_res)
### PREPARE NEW CLICK DATA
# Example: new_clicks is your unlabeled dataset
# Ensure same feature set and types as training
new_clicks <- new_clicks_raw %>%
  mutate(
    # any preprocessing you used during training
    # e.g., log transforms, scaling, factor levels, etc.
  )

##RUN THE HELPER AND GET PREDICTIONS
rf_results <- predict_click_species(
  newdata = new_clicks,
  rf_mod  = rf_mod,
  feat    = feat
)

head(rf_results)

##save
write.csv(rf_results,
          "new_clicks_with_rf_species.csv",
          row.names = FALSE)