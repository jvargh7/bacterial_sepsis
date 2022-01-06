# ngal.rec ----------
ngal_rec <- foreign::read.epiinfo(paste0(path_bs_folder,"/working/NGAL.rec"))
attr_ngal <- attr(ngal_rec,"prompts") %>% 
  imap_dfr(.,.f=function(x,name) data.frame(var = name,
                                            label = x)) %>% 
  mutate(label = str_replace_all(label,"(\\s){2,}","\t")) %>% 
  separate(label,into=c("orig_var","description"),sep="\t",remove = FALSE) %>% 
  mutate(orig_var = case_when(var == "ANTIBIOTI1" ~ "antibiotics_used",
                              var == "DURATIONDU" ~ "duration_hospital_stay",
                              var == "DURATIOND1" ~ "duration_prior_antibiotic",
                              var == "OTHEROTHER" ~ "other_morbidity",
                              var == "OTHEROTHE1" ~ "other_antibiotic",
                              var == "OTHER22OTH" ~ "other_antibiotic2",
                              TRUE ~ orig_var
  ))


# ngal -----------

ngal <- readxl::read_excel(paste0(path_bs_folder,"/working/NGAL.xlsx"))

names(ngal) <- attr_ngal$orig_var

labelled::var_label(ngal) <- attr_ngal$description

ngal_old <- ngal %>% 
  as.tibble() %>% 
  dplyr::select(-name) %>% 
  rename(periferies_cold = periferies,
         'type_of_vent' = 'type of vent',
         'inotrop_4h' = '4h inotrop',
         'hospital_number'='h.no',
         'case_control' = 'case/control',
         'male' = 'sex') %>%
  rename_all(.f = function(x) str_replace_all(x,"\\s","_") %>% str_to_lower(.)) %>% 
  rename_at(vars(male,myalgia,chills,
                 rigors,cns,
                 respi,uti,gi,softtiss,
                 dm,htn,cancer,cad,copd,
                 steroid,immunocomp,pregnant,other_morbidity,
                 antibiotics,jaundice,periferies_cold,edema,
                 culture,sepsis,hai,icu,
                 ventillation,
                 piptaz,azithro,mero,vanco,
                 ceftria,clinda,doxy,
                 colistin, linezolid,
                 other_antibiotic,
                 piptaz2,azithro2,mero2,
                 vanco2,ceftria2,clinda2,doxy2,
                 colistin2,linezolid2,
                 other_antibiotic2,inotropes,inotrop_4h,aki,dialysis),function(x) paste0("d_",x)) %>% 
  mutate_at(vars(starts_with("d_")),
            function(x) case_when(x == 1 ~ 1,
                                  x == 2 ~ 0,
                                  TRUE ~ NA_real_)) %>% 
  mutate(sepsis_outcome = case_when(d_culture == 1 ~ 1,
                                    case_control == 1 ~ 2,
                                    case_control == 2 ~ 3,
                                    case_control == 3 ~ 4,
                                    TRUE ~ NA_real_)) %>%
  mutate(
         sepsis_outcome = factor(sepsis_outcome,levels=c(1:4),labels=c("Blood culture positive",
                                                                 "Suspected bacteremia",
                                                                 "Non-bacteremic patients",
                                                                 "Ambiguous group"),
                              ordered =TRUE),
         d_pulse_gt90 = case_when(pulse > 90 ~ 1,
                                  !is.na(pulse) & pulse <= 90 ~ 0,
                                  TRUE ~ NA_real_),
         d_sysbp_lt90 = case_when(sysbp < 90 ~ 1,
                                  !is.na(sysbp) & sysbp >= 90 ~ 0,
                                  TRUE ~ NA_real_),
         d_rr_gt22 = case_when(rr > 22 ~ 1,
                               !is.na(rr) & rr <= 22 ~ 0,
                               TRUE ~ NA_real_),
         d_gcs_lt15 = case_when(gcs < 15 ~ 1,
                                !is.na(gcs) & gcs >= 15 ~ 0,
                                TRUE ~ NA_real_),
         d_sirs_score_gt3 = case_when(sirs_score > 3 ~ 1,
                                      !is.na(sirs_score) & sirs_score <= 3 ~ 0,
                                      TRUE ~ NA_real_),
         d_qsofa_gt2 = case_when(qsofa > 2 ~ 1,
                                 !is.na(qsofa) & qsofa <= 2 ~ 0,
                                 TRUE ~ NA_real_),
         d_tc_gt12k = case_when(tc > 12000 ~ 1,
                                !is.na(tc) & tc <= 12000 ~ 0,
                                TRUE ~ NA_real_),
         tc_10e3 = tc/1000,
         d_dc_gt80 = case_when(dc > 80 ~ 1,
                               !is.na(dc) & dc <= 80 ~ 0,
                               TRUE ~ NA_real_),
         d_plts_lt10k = case_when(plts < 10000 ~ 1,
                                  !is.na(plts) & plts >= 10000 ~ 0,
                                  TRUE ~ NA_real_),
         plts_10e3 = plts/1000,
         d_creatine_gt2 = case_when(creatine > 2 ~ 1,
                                    !is.na(creatine) & creatine <= 2 ~ 0,
                                    TRUE ~ NA_real_),
         d_lactate_gt2 = case_when(lactate > 2 ~ 1,
                                   !is.na(lactate) & lactate <= 2 ~ 0,
                                   TRUE ~ NA_real_),
         d_tb_gt2 = case_when(tb > 2 ~ 1,
                              !is.na(tb) & tb <= 2 ~ 0,
                              TRUE ~ NA_real_),
         d_sgot_gt40 = case_when(sgot > 40 ~ 1,
                                 !is.na(sgot) & sgot <= 40 ~ 0,
                                 TRUE ~ NA_real_),
         d_sgpt_gt41 = case_when(sgpt > 41 ~ 1,
                                 !is.na(sgpt) & sgpt <= 41 ~ 0,
                                 TRUE ~ NA_real_)
         
         
  )

