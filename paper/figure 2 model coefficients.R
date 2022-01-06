last_rf_vip <- readRDS(paste0(path_bs_folder,"/working/last_rf_vip.RDS"))

vif_B <- last_rf_vip$data %>% 
  mutate(description = case_when(Variable == "d_source" ~ "Any focus of infection",
                                 Variable == "creatine" ~ "Creatine (mg/dL)",
                                 
                                 
                                 Variable == "age" ~ "Age in years",
                                 Variable == "dc" ~ "Differential count (cells/mm3)",
                                 Variable == "ngal100" ~ "Plasma NGAL in 100 mg/dL",
                                 Variable == "sgot" ~ "SGOT",
                                 Variable == "plts_10e3" ~ "Platelets x 10^3 (cells/mm3)",
                                 Variable == "d_creatine_gt2" ~ "Creatine > 2",
                                 Variable == "pulse" ~ "Heart rate per min",
                                 Variable == "d_uti" ~ "UTI",
                                 Variable == "tc_10e3" ~ "Total count (cells/mm3)",
                                 Variable == "d_rigor" ~ "Rigors",
                                 Variable == "gcs" ~ "GCS",
                                 Variable == "sgpt" ~ "SGPT in ?",
                                 Variable == "tb" ~ "Total bilirubin (mg/dL)",
                                 Variable == "lactate" ~ "Lactate (mmol/L)",
                                 Variable == "sodium" ~ "Sodium in ?",
                                 Variable == "sirs_score" ~ "SIRS score",
                                 Variable == "d_dm" ~ "Diabetes",
                                 Variable == "d_sirs_score_gt3" ~ "SIRS Score > 3",
                                 Variable == "ph" ~ "pH",
                                 Variable == "d_myalgia" ~ "Myalgia",
                                 Variable == "d_respi" ~ "Breathlessness",
                                 TRUE ~ NA_character_) ) %>% 
  mutate(description = fct_reorder(description,Importance)) %>% 
  ggplot(data=.,aes(y=description,x=Importance)) +
  geom_col() +
  ylab("") +
  theme_bw()


last_glmnet_fit <- readRDS(paste0(path_bs_folder,"/working/last_glmnet_fit.RDS"))


estimate_A <- last_glmnet_fit %>% 
  extract_fit_parsnip() %>% 
  tidy() %>% 
  dplyr::filter(term != "(Intercept)") %>% 
  arrange(-abs(estimate)) %>% 
  slice(1:20) %>% 
  mutate(description = case_when(term == "d_source" ~ "Any focus of infection",
                                 term == "d_dm" ~ "Diabetes",
                                 term == "d_sirs_score_gt3" ~ "SIRS > 3",
                                 term == "d_gi" ~ "Vomiting/Diarrhea/Nausea",
                                 term == "d_creatine_gt2" ~ "Creatinine > 2",
                                 term == "age" ~ "Age",
                                 term == "d_rigors" ~ "Rigors",
                                 term == "d_gcs_lt15" ~ "GCS < 15",
                                 term == "d_uti" ~ "UTI",
                                 term == "sgot" ~ "SGOT",
                                 term == "creatine" ~ "Creatinine",
                                 term == "d_qsofa_gt2" ~ "QSOFA > 2",
                                 term == "d_pulse_gt90" ~ "Pulse > 90",
                                 term == "d_htn" ~ "Hypertension",
                                 term == "d_softtiss" ~ "Soft tissue injury",
                                 term == "d_respi" ~ "Breathlessness",
                                 term == "rr" ~ "Respiration rate",
                                 term == "d_plts_lt10k" ~ "Plateles < 10K",
                                 term == "lactate" ~ "Lactate",
                                 
                                 term == "d_myalgia" ~ "Myalgia",
                                 
                                 term == "d_cns" ~ "Seizures/Altered sensorium",
                                 term == "d_cad" ~ "Coronary Artery Disease",
                                 term == "d_copd" ~ "COPD",
                                 term == "sysbp" ~ "Systolic Blood Pressure",
                                 term == "d_steroid" ~ "Chronic steroid use",
                                 term == "pulse" ~ "Heart rate",
                                 term == "d_male" ~ "Male",
                                 term == "d_immunocomp" ~ "Immunocompromised",
                                 term == "fever" ~ "Fever duration days",
                                 TRUE ~ NA_character_

                                 )) %>% 
  mutate(description = fct_reorder(description,estimate)) %>% 
  ggplot(data=.,aes(x=estimate,y=description)) +
  geom_point() +
  theme_bw() +
  geom_vline(xintercept = 0,linetype = 2,col="red") +
  xlab("Final GLMNET estimate") +
  ylab("")


library(ggpubr)

ggarrange(estimate_A,
          vif_B,
          ncol=2,nrow=1,labels=c("A","B"))
