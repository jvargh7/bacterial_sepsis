
# https://www.tidymodels.org/start/tuning/
# BELOW ARE WHEN WE ARE EVALUATING ONLY Logit
# 4.1 Finalizing our model --------
final_glmnet_wflow <- 
  glmnet_wflow %>% 
  finalize_workflow(best_fit)

final_glmnet_fit <- 
  final_glmnet_wflow %>%
  last_fit(ngal_split) 

# 5. Testing the model ---------

final_glmnet_fit %>%
  collect_metrics()

final_glmnet_fit %>%
  collect_predictions() %>% 
  roc_curve(truth=sepsis_2cat, .pred_Negative) %>% 
  autoplot()

# The final_fit object contains a finalized, fitted workflow that 
# you can use for predicting on new data or further understanding the results. 
# You may want to extract this object, using one of the extract_ helper functions.

final_glmnet_wflow <- extract_workflow(final_glmnet_fit)
final_glmnet_wflow

# We can create a visualization of the decision tree using another helper 
# function to extract the underlying engine-specific fit.

final_glmnet_wflow %>% 
  extract_fit_engine() %>% 
  plot(.)
