last_glmnet_fit <- readRDS(paste0(path_bs_folder,"/working/last_glmnet_fit_filt.RDS"))


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
                                 term == "plts_10e3" ~ "Platelets x 10^3 (cells/mm3)",
                                 term == "lactate" ~ "Lactate",
                                 term == "ngal100" ~ "Plasma NGAL in 100 mg/dL",
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


last_rf_vip <- readRDS(paste0(path_bs_folder,"/working/last_rf_vip_filt.RDS"))

vif_B <- last_rf_vip$data %>% 
  mutate(description = case_when(Variable == "d_source" ~ "Any focus of infection",
                                 Variable == "d_dm" ~ "Diabetes",
                                 Variable == "d_sirs_score_gt3" ~ "SIRS > 3",
                                 Variable == "d_gi" ~ "Vomiting/Diarrhea/Nausea",
                                 Variable == "d_creatine_gt2" ~ "Creatinine > 2",
                                 Variable == "age" ~ "Age",
                                 Variable == "d_rigors" ~ "Rigors",
                                 Variable == "d_gcs_lt15" ~ "GCS < 15",
                                 Variable == "d_uti" ~ "UTI",
                                 Variable == "sgot" ~ "SGOT",
                                 Variable == "creatine" ~ "Creatinine",
                                 Variable == "d_qsofa_gt2" ~ "QSOFA > 2",
                                 Variable == "d_pulse_gt90" ~ "Pulse > 90",
                                 Variable == "d_htn" ~ "Hypertension",
                                 Variable == "d_softtiss" ~ "Soft tissue injury",
                                 Variable == "d_respi" ~ "Breathlessness",
                                 Variable == "rr" ~ "Respiration rate",
                                 Variable == "d_plts_lt10k" ~ "Plateles < 10K",
                                 Variable == "plts_10e3" ~ "Platelets x 10^3 (cells/mm3)",
                                 Variable == "lactate" ~ "Lactate",
                                 Variable == "ngal100" ~ "Plasma NGAL in 100 mg/dL",
                                 Variable == "d_myalgia" ~ "Myalgia",
                                 
                                 Variable == "d_cns" ~ "Seizures/Altered sensorium",
                                 Variable == "d_cad" ~ "Coronary Artery Disease",
                                 Variable == "d_copd" ~ "COPD",
                                 Variable == "sysbp" ~ "Systolic Blood Pressure",
                                 Variable == "d_steroid" ~ "Chronic steroid use",
                                 Variable == "pulse" ~ "Heart rate",
                                 Variable == "d_male" ~ "Male",
                                 Variable == "d_immunocomp" ~ "Immunocompromised",
                                 Variable == "fever" ~ "Fever duration days",
                                 TRUE ~ NA_character_) ) %>% 
  mutate(description = fct_reorder(description,Importance)) %>% 
  ggplot(data=.,aes(y=description,x=Importance)) +
  geom_col() +
  ylab("") +
  theme_bw()




library(ggpubr)

ggarrange(estimate_A,
          vif_B,
          ncol=2,nrow=1,labels=c("A","B")) %>% 
  ggsave(.,filename=paste0(path_bs_folder,"/figures/model coefficients.png"))

