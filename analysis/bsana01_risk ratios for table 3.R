ngal_clean <- readRDS(paste0(path_bs_folder,"/working/ngal_clean.RDS")) %>% 
  mutate(d_sepsis_2cat = case_when(sepsis_2cat == "Positive" ~ 1,
                                   sepsis_2cat == "Negative" ~ 0,
                                   TRUE ~ NA_real_),
         ngal100 = ngal/100)

# https://stats.stackexchange.com/questions/520662/how-to-add-robust-error-variances-in-glm-poisson-model-in-r
library("sandwich")
library("lmtest")
# coeftest(model, vcov = sandwich)

m1 <- glm(d_sepsis_2cat ~ ngal100,data=ngal_clean,family = poisson())  %>% 
  coeftest(., vcov = sandwich) %>% 
  broom::tidy() %>% 
  mutate(coef = round(exp(estimate),2),
         lci = round(exp(estimate - 1.96*std.error),2),
         uci = round(exp(estimate + 1.96*std.error),2))

m2 <- glm(d_sepsis_2cat ~ ngal100 + creatine + age,data=ngal_clean,family = poisson()) %>% 
  coeftest(., vcov = sandwich) %>% 
  broom::tidy() %>% 
  mutate(coef = round(exp(estimate),2),
         lci = round(exp(estimate - 1.96*std.error),2),
         uci = round(exp(estimate + 1.96*std.error),2))

m3 <- glm(d_sepsis_2cat ~ ngal100 + creatine + age + d_rigors,
             data=ngal_clean,family = poisson()) %>%
  coeftest(., vcov = sandwich) %>% 
  broom::tidy() %>% 
  mutate(coef = round(exp(estimate),2),
         lci = round(exp(estimate - 1.96*std.error),2),
         uci = round(exp(estimate + 1.96*std.error),2))

bind_rows(m1 %>% mutate(model = "Model 1"),
          m2 %>% mutate(model = "Model 2"),
          m3 %>% mutate(model = "Model 3")) %>% 
  mutate(coef_ci = paste0(coef," (",
                          lci,", ",
                          uci,")")) %>%
  dplyr::filter(term!="(Intercept)") %>% 
dplyr::select(model,term,coef_ci) %>% 
  pivot_wider(names_from=model,values_from=coef_ci) %>% 
  write.csv(.,paste0(path_bs_folder,"/working/table 3.csv"))



