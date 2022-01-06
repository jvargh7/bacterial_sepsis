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
# Outcome (4cat) for 854042f is different between final analysis 
# - reviewer data.xlsx and NGAL for Jithin analysis.xlsx

ngal <- readxl::read_excel(paste0(path_bs_folder,"/working/NGAL for GMV only cases and controls.xlsx"))
bacteremia_4cat <- readxl::read_excel(paste0(path_bs_folder,"/working/final analysis with reviewer data.xlsx")) %>% 
  dplyr::select(one_of('BACTERMIA=1, Culture negative sepsis =2, other diagnosis=3,,doubtful =4')) %>% 
  pull()
reviewer1 <- readxl::read_excel(paste0(path_bs_folder,"/working/final analysis with reviewer data.xlsx")) %>% 
  dplyr::select(one_of('independent reviewer 1 (1=bacteremia, 2=non bacterial sepsis)')) %>% 
  pull()
reviewer2 <- readxl::read_excel(paste0(path_bs_folder,"/working/final analysis with reviewer data.xlsx")) %>% 
  dplyr::select(one_of('independent reviewer 2 bacterial =1, and non bacterail =2')) %>% 
  pull()
reviewer3 <- readxl::read_excel(paste0(path_bs_folder,"/working/final analysis with reviewer data.xlsx")) %>% 
  dplyr::select(one_of('3rd reviewer to resolve conflict')) %>% 
  pull()


names(ngal) <- attr_ngal$orig_var

labelled::var_label(ngal) <- attr_ngal$description
  
ngal_clean <- ngal %>% 
  as.tibble() %>% 
  dplyr::select(-name) %>% 
  rename(periferies_cold = periferies,
         'type_of_vent' = 'type of vent',
         'inotrop_4h' = '4h inotrop',
         'hospital_number'='h.no',
         'sepsis_2cat' = 'case/control',
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
  mutate(sepsis_4cat = bacteremia_4cat,
         r1_classification = reviewer1,
         r2_classification = reviewer2,
         r3_classification = reviewer3) %>% 
  mutate(sepsis_2cat = case_when(bacteremia_4cat == 1 ~ 1,
                                 bacteremia_4cat == 3 ~ 2,
                                 bacteremia_4cat %in% c(2,4) &
                                   (r1_classification == r2_classification) ~ r1_classification,
                                 bacteremia_4cat %in% c(2,4) &
                                   (r1_classification != r2_classification) ~ r3_classification,
                                 TRUE ~ NA_real_
                                 ),
         sepsis_4cat = case_when(bacteremia_4cat == 1 ~ 1,
                                 bacteremia_4cat == 3 ~ 3,
                                 bacteremia_4cat %in% c(2,4) &
                                   (r1_classification == r2_classification) &
                                   r1_classification == 1 ~ 2,
                                 bacteremia_4cat %in% c(2,4) &
                                   (r1_classification != r2_classification) &
                                   r3_classification == 1 ~ 2,
                                 TRUE ~ 4
                                 )
         ) %>%
  mutate(sepsis_2catsens = case_when(sepsis_4cat %in% c(1,2) ~ 1,
                                     sepsis_4cat %in% c(3) ~ 2,
                                     TRUE ~ NA_real_)) %>% 
  mutate(sepsis_2cat = factor(sepsis_2cat,levels=c(1:2),labels=c("Positive",
                                                                 "Negative"),
                              ordered =TRUE),
         sepsis_2catsens = factor(sepsis_2catsens,levels=c(1:2),labels=c("Positive",
                                                                 "Negative"),
                              ordered =TRUE),
         sepsis_4cat = factor(sepsis_4cat,levels=c(1:4),labels=c("Positive via blood culture",
                                                              "Positive via physician diagnosis",
                                                              "Negative via alternate diagnosis",
                                                              "Negative via physician diagnosis"),
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
                                 TRUE ~ NA_real_),
         d_source = case_when(source %in% c(1:4) ~ 1,
                              source == 5 ~ 0,
                              TRUE ~ NA_real_)
         
         
         )

saveRDS(ngal_clean,paste0(path_bs_folder,"/working/ngal_clean.RDS"))
xlsx::write.xlsx(ngal_clean,paste0(path_bs_folder,"/working/ngal_clean.xlsx"))
