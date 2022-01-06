
library(tidymodels)
library(dotwhisker)

# 1 Build the model -------
# https://www.tidymodels.org/start/recipes/


glmnet_mod <- logistic_reg(penalty=tune(),mixture = tune()) %>% 
  set_engine("glmnet")

# 2 Using library(recipes) ---------
# source(paste0(path_bs_repo,"/analysis/bspredaux01_recipe for model.R"))

# 3 Building a model and recipe workflow ---------

glmnet_wflow <- workflow() %>% 
  add_model(glmnet_mod) %>% 
  add_recipe(ngal_rec)


# 4. Fitting a model with resampling ------
# https://www.tidymodels.org/start/resampling/
# Tuning in tidymodels requires a resampled object created with the rsample package..
set.seed(2021)

# Resampling is always used with the training set. 

# folds <- vfold_cv(ngal_train,v=3,repeats=3)
folds <- bootstraps(ngal_train,times = 10,strata=sepsis_2cat)

set.seed(2021)
glmnet_rs <- glmnet_wflow %>% 
  tune_grid(resamples=folds,
            grid = glmnet_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc,accuracy))


glmnet_rs %>% 
  show_best(metric = "roc_auc")
glmnet_rs %>% 
  show_best(metric = "accuracy")
collect_metrics(glmnet_rs)


glmnet_rs %>%
  collect_metrics() %>%
  mutate(mixture = factor(mixture)) %>%
  ggplot(aes(penalty, mean, color = mixture)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)



# # https://www.tidymodels.org/start/tuning/
# best_fit <- glmnet_rs %>%
#   select_best("accuracy")

# Our model performance seems to plateau at the smaller penalty values, 
# so going by the roc_auc metric alone could lead us to multiple options 
# for the “best” value for this hyperparameter:
# However, we may want to choose a penalty value further along the x-axis, 
# closer to where we start to see the decline in model performance. 
# https://www.tidymodels.org/start/case-study/

glmnet_rs_table <- 
  glmnet_rs %>% 
  collect_metrics() %>% 
  arrange(penalty)

lr_best <- 
  glmnet_rs %>% 
  collect_metrics() %>% 
  arrange(penalty) %>% 
  # slice(10) %>% 
  dplyr::filter(mixture == 0, penalty == 0.1)

glmnet_rs %>% 
  collect_predictions(parameters = lr_best) %>% 
  roc_curve(sepsis_2cat, .pred_Negative) %>% 
  mutate(model = "Logistic Regression") %>% 
  autoplot()

glmnet_auc <- 
  glmnet_rs %>% 
  collect_predictions(parameters = lr_best) %>% 
  roc_curve(sepsis_2cat, .pred_Negative) %>% 
  mutate(model = "Elasticnet Regression")

autoplot(glmnet_auc)
