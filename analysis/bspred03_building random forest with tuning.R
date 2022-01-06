
library(tidymodels)
library(dotwhisker)

# 1 Build the model -------
# https://www.tidymodels.org/start/tuning/


rf_mod <- rand_forest(mtry=tune(),min_n = tune(),trees=tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")


# 2 Using library(recipes) ---------
# source(paste0(path_bs_repo,"/analysis/bspredaux01_recipe for model.R"))


# 3 Building a model and recipe workflow ---------

rf_wflow <- workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(ngal_rec)


# 4. Fitting a model with resampling ------
# https://www.tidymodels.org/start/resampling/

set.seed(2021)

rf_res <- rf_wflow %>% 
  tune_grid(resamples=folds,
            # grid = 25, # We will use a space-filling design to tune, with 25 candidate models:
            grid = rf_grid, 
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc,accuracy)
            )

collect_metrics(rf_res)

# The message printed above “Creating pre-processing data to finalize 
# unknown parameter: mtry” is related to the size of the data set. 
# Since mtry depends on the number of predictors in the data set, 
# tune_grid() determines the upper bound for mtry once it receives the data.
# 
# Here are our top 5 random forest models, out of the 25 candidates:
rf_res %>% 
  show_best(metric = "roc_auc")
rf_res %>% 
  show_best(metric = "accuracy")
# Plotting the results of the tuning process highlights that 
# both mtry (number of predictors at each node) and 
# min_n (minimum number of data points required to keep splitting) 
# should be fairly small to optimize performance. 

autoplot(rf_res)

rf_best <- 
  rf_res %>% 
  select_best(metric = "roc_auc")

rf_auc <- 
  rf_res %>% 
  collect_predictions(parameters = rf_best) %>% 
  roc_curve(sepsis_2cat, .pred_Negative) %>% 
  mutate(model = "Random Forest")
