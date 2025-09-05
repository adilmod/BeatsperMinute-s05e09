library(tidyverse)
library(tidymodels)
library(vip)
library(xgboost)

set.seed(2179)

musicSplit <- initial_split(train, prop = 0.8, strata = BeatsPerMinute)


musicSplit$data$BeatsPerMinute <- log(musicSplit$data$BeatsPerMinute, base=10)

musicTrain <- training(musicSplit)
musicTest <- testing(musicSplit)

skim(musicTrain)

linear_reg_glmnet_spec <-
  linear_reg(mode="regression", penalty = 0.01, mixture = 0) %>%
  set_engine('glmnet')

xgboostspec <- 
  boost_tree(
    trees = 1000,
    tree_depth = 6,
    min_n = 5,
    loss_reduction = 0.01,
    sample_size = 0.8,
    mtry = 5,
    learn_rate = 0.3,
    stop_iter = 10
  ) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

musicRecipe <- recipe(BeatsPerMinute ~ ., data = musicTrain) %>%
  step_normalize(AudioLoudness, TrackDurationMs) %>%
  step_nzv(all_predictors())


musicWF <- workflow() %>%
  add_model(xgboostspec) %>%
  add_recipe(musicRecipe)

modelFit <- fit(musicWF, data = musicTrain)

musicTestRes <- predict(modelFit, new_data = musicTest) %>%
  bind_cols(musicTest %>% select(BeatsPerMinute))


musicTestMetrics <- metric_set(rmse, rsq, mae)
musicTestMetrics(musicTestRes, truth = BeatsPerMinute, estimate = .pred)