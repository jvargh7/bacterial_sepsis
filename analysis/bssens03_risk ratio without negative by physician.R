ngal_clean <- readRDS(paste0(path_bs_folder,"/working/ngal_clean.RDS")) %>% 
  dplyr::filter(!is.na(sepsis_2catsens)) %>% 
  mutate(sepsis_2catsens = factor(sepsis_2catsens,levels=c("Negative","Positive")),
         ngal100 = ngal/100) %>% 
  dplyr::select(-sepsis_2cat) %>% 
  rename(sepsis_2cat = sepsis_2catsens) %>% 
  mutate(d_sepsis_2cat = case_when(sepsis_2cat == "Positive" ~ 1,
                                   sepsis_2cat == "Negative" ~ 0,
                                   TRUE ~ NA_real_))

glm(data=ngal_clean,d_sepsis_2cat ~ ngal100,family=binomial()) %>% 
 broom::tidy(exponentiate = TRUE)


library(geepack)

m1 <- geeglm(d_sepsis_2cat ~ ngal100,data=ngal_clean,family = poisson(),id = serialno,std.err = "san.se") %>% 
  broom::tidy() %>% 
  mutate(coef = round(exp(estimate),2),
         lci = round(exp(estimate - 1.96*std.error),2),
         uci = round(exp(estimate + 1.96*std.error),2))

m2 <- geeglm(d_sepsis_2cat ~ ngal100 + creatine + age,data=ngal_clean,family = poisson(),id = serialno,std.err = "san.se") %>% 
  broom::tidy() %>% 
  mutate(coef = round(exp(estimate),2),
         lci = round(exp(estimate - 1.96*std.error),2),
         uci = round(exp(estimate + 1.96*std.error),2))

m3 <- geeglm(d_sepsis_2cat ~ ngal100 + creatine + age + dc,data=ngal_clean,family = poisson(),id = serialno,std.err = "san.se") %>% 
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
  write.csv(.,paste0(path_bs_folder,"/working/supplementary table 4.csv"))
