
# 0 Train-test split @ 80/20 ----------
ngal_clean <- readRDS(paste0(path_bs_folder,"/working/ngal_clean.RDS")) %>% 
  mutate(sepsis_2cat = factor(sepsis_2cat,levels=c("Negative","Positive")),
         ngal100 = ngal/100)

set.seed(2021)

ngal_split <- initial_split(ngal_clean, 
                            strata = sepsis_2cat,prop=0.80)
saveRDS(ngal_split,paste0(path_bs_folder,"/working/sens_split.RDS"))

ngal_train <- training(ngal_split)
# ngal_valsplit <- validation_split(ngal_train,
#                                   strata=sepsis_2cat,
#                                   prop=0.9)
ngal_test  <- testing(ngal_split)


# 2 Using library(recipes) ---------

# View(ngal_rec$var_info)

# Variables in recipes can have any type of role in subsequent analyses such as: 
# outcome, predictor, case weights, stratification variables, etc.

# We use ngal_train which is the original training data in the recipe
# This is instead of the true training data from validation_split(ngal_train)
ngal_rec <- recipe(data = ngal_train,
                   sepsis_2cat ~ age + 
                     d_dm + d_uti +
                     d_source +
                     d_rigors + d_myalgia + 
                     d_gcs_lt15 + 
                     d_sirs_score_gt3 +
                     d_qsofa_gt2 +
                     
                     plts_10e3 + d_plts_lt10k + 
                     creatine + d_creatine_gt2 + 
                     ngal100) %>% 
  # step_impute_linear(all_predictors(),impute_with = imp_vars(all_predictors())) %>% 
  step_impute_knn(all_predictors(),impute_with = imp_vars(all_predictors())) %>%
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# 4. Fitting a model with resampling ------

# Tuning in tidymodels requires a resampled object created with the rsample package..
set.seed(2021)

# Resampling is always used with the training set. 

# folds <- vfold_cv(ngal_train,v=3,repeats=3)
folds <- bootstraps(ngal_train,times = 10,strata=sepsis_2cat)
saveRDS(folds,paste0(path_bs_folder,"/working/filt_folds.RDS"))

