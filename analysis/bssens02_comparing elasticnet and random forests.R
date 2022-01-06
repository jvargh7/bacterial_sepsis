library(tidymodels)

source(paste0(path_bs_repo,"/analysis/bssens01_recipe for model without negative by physician.R"))

# We can’t train this specification on a single data set 
# (such as the entire training set) and learn what the hyperparameter values should be,
# but we can train many models using resampled data and see which models turn out best.
# We can create a regular grid of values to try using some convenience functions for 
# each hyperparameter:

# glmnet_grid <- grid_regular(penalty(),
#                             mixture())
# 
# rf_grid <- grid_regular(mtry(range=c(3,7)),
#                         min_n(),
#                         trees())

glmnet_grid <- expand.grid(penalty = c(0.00001,0.001,0.1,1),
                           mixture = c(0,0.5,1))

rf_grid <- expand.grid(mtry = c(3,5,7),
                       min_n = c(10,20,30),
                       trees = c(500,1000,1500,2000))

source(paste0(path_bs_repo,"/analysis/bspred02_building elasticnet with tuning.R"))
saveRDS(glmnet_rs,paste0(path_bs_folder,"/working/glmnet_rs_sens.RDS"))


source(paste0(path_bs_repo,"/analysis/bspred03_building random forest with tuning.R"))
saveRDS(rf_res,paste0(path_bs_folder,"/working/rf_res_sens.RDS"))

# https://www.tidymodels.org/start/case-study/
# The random forest is uniformly better across event probability thresholds.
bind_rows(rf_auc, glmnet_auc) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) + 
  geom_path(lwd = 1.5, alpha = 0.8) +
  geom_abline(lty = 3) + 
  coord_equal() + 
  # scale_color_viridis_d(option = "plasma", end = .6) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_color_manual(name = "",values=c("darkgreen","red")) +
  xlab("1- Specificity") +
  ylab("Sensitivity")

# Both models were comparable
# We choose the random forest 

# the last model
last_rf_mod <- 
  rand_forest(mtry = 3, min_n = 10, trees = 2000) %>% 
  set_engine("ranger", 
             # num.threads = cores, 
             importance = "impurity") %>% 
  set_mode("classification")

last_glmnet_mod <- 
  logistic_reg(penalty=0.1,mixture = 0,mode = "classification") %>% 
  set_engine("glmnet")


# the last workflow
last_rf_wflow <- 
  rf_wflow %>% 
  update_model(last_rf_mod)

last_glmnet_wflow <- 
  glmnet_wflow %>% 
  update_model(last_glmnet_mod)


# the last fit: accuracy and AUC -------
set.seed(345)
last_rf_fit <- 
  last_rf_wflow %>% 
  last_fit(ngal_split)
last_rf_fit %>% 
  collect_metrics()
saveRDS(last_rf_fit,paste0(path_bs_folder,"/working/last_rf_fit_sens.RDS"))

last_glmnet_fit <- 
  last_glmnet_wflow %>% 
  last_fit(ngal_split)
last_glmnet_fit %>% 
  collect_metrics()
saveRDS(last_glmnet_fit,paste0(path_bs_folder,"/working/last_glmnet_fit_sens.RDS"))


# Variable importance -------
last_rf_vip <- last_rf_fit %>% 
  pluck(".workflow", 1) %>%   
  extract_fit_parsnip() %>% 
  vip::vip(num_features = 20)

last_rf_vip$data %>% 
  ggplot(data=.,aes(x=))

saveRDS(last_rf_vip,paste0(path_bs_folder,"/working/last_rf_vip_sens.RDS"))

# ROC on test set ---------
last_rf_fit %>% 
  collect_predictions() %>% 
  roc_curve(sepsis_2cat, .pred_Negative) %>% 
  autoplot()
