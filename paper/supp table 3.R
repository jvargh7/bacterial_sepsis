rf_res <- readRDS(paste0(path_bs_folder,"/working/rf_res_sens.RDS"))
glmnet_rs <- readRDS(paste0(path_bs_folder,"/working/glmnet_rs_sens.RDS"))
last_rf_fit <- readRDS(paste0(path_bs_folder,"/working/last_rf_fit_sens.RDS"))
last_glmnet_fit <- readRDS(paste0(path_bs_folder,"/working/last_glmnet_fit_sens.RDS"))


library(caret)
with(last_glmnet_fit$.predictions[[1]],
     confusionMatrix(.pred_class,reference=sepsis_2cat))
with(last_rf_fit$.predictions[[1]],
     confusionMatrix(.pred_class,reference=sepsis_2cat))

confusionMatrix(reference=last_glmnet_fit$.predictions[[1]]$.pred_class,
                last_rf_fit$.predictions[[1]]$.pred_class)

library(tidymodels)
rf_best <- 
  rf_res %>% 
  select_best(metric = "roc_auc")

lr_best <- 
  glmnet_rs %>% 
  collect_metrics() %>% 
  arrange(penalty) %>% 
  dplyr::filter(penalty == 0.1,
                mixture == 0) # alpha = 0.1 and mixture = 0


stab3_train <- bind_rows(rf_res %>% 
                          collect_metrics() %>% 
                          dplyr::filter(mtry == rf_best$mtry,
                                        trees == rf_best$trees,
                                        min_n == rf_best$min_n) %>% 
                          mutate(parameters = paste0('mtry = ',mtry,"/",
                                                     'trees = ',trees,"/",
                                                     'min_n = ',min_n),
                                 model = "Elastic net",
                                 data = "Train"),
                        glmnet_rs %>% 
                          collect_metrics() %>% 
                          dplyr::filter(penalty == lr_best$penalty,
                                        mixture == lr_best$mixture) %>% 
                          mutate(parameters = paste0('penalty = ',penalty,"/",
                                                     'mixture = ',mixture),
                                 model = "Random Forests",
                                 data = "Train")
                        
) %>% 
  mutate(mean_ci = paste0(round(mean,2)," (",
                          round(mean - 1.96*std_err,2),",",
                          round(mean + 1.96*std_err,2),")")) %>% 
  dplyr::select(parameters,model,data,.metric,mean_ci)


stab3_test <- bind_rows(last_glmnet_fit %>% 
                         collect_metrics() %>% 
                         mutate(model = "Elastic net",
                                data = "Test"),
                       last_rf_fit %>% 
                         collect_metrics() %>% 
                         mutate(model = "Random Forests",
                                data = "Test")
) %>% 
  mutate(mean_ci = round(.estimate,2) %>% as.character(.))

stab3_df <- bind_rows(stab3_train,
                     stab3_test) %>% 
  dplyr::select(model,data,.metric,mean_ci)  %>% 
  pivot_wider(names_from=c(.metric,data),values_from=mean_ci)

stab3_df %>% 
  write.csv(.,paste0(path_bs_folder,"/working/supplementary table 3.csv"),row.names = FALSE)
