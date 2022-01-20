ngal_clean <- readRDS(paste0(path_bs_folder,"/working/ngal_clean.RDS"))

library(compareGroups)

ngal_clean %>% 
  compareGroups(sepsis_4cat ~ age + d_male + sepsis_2cat +
                  d_dm + d_htn + d_copd + d_cad +
                  d_steroid + d_immunocomp +
                  fever + d_respi + d_uti +
                  d_gi + d_cns + d_softtiss +
                  d_source + 
                  d_rigors + d_myalgia + 
                  
                  pulse + d_pulse_gt90 +
                  sysbp + d_sysbp_lt90 +
                  diabp +
                  
                  rr + d_rr_gt22 +
                  gcs + d_gcs_lt15 + 
                  sirs_score + d_sirs_score_gt3 +
                  qsofa + d_qsofa_gt2 + d_periferies_cold +
                  
                  tc_10e3 + d_tc_gt12k + 
                  dc + d_dc_gt80 +
                  plts_10e3 + d_plts_lt10k + 
                  creatine + d_creatine_gt2 + 
                  lactate + d_lactate_gt2 +
                  sodium + ph +
                  tb + d_tb_gt2 +
                  sgot + d_sgot_gt40 +
                  sgpt + d_sgpt_gt41 +
                  ngal,
                data=.,
                method =  c(1,3,3,
                            3,3,3,3,
                            3,3,
                            2,3,3,
                            3,3,3,
                            3,
                            3,3,
                            
                            1,3,
                            1,3,
                            1,
                            
                            2,3,
                            2,3,
                            2,3,
                            2,3,3,
                            
                            1,3,
                            1,3,
                            1,3,
                            2,3,
                            2,3,
                            1,1,
                            2,3,
                            1,3,
                            1,3,
                            1
                            )) %>% 
  createTable(digits = 1,show.all = TRUE,show.n = TRUE,
              q.type = c(2,2),type = 1,sd.type = 2) %>% 
  export2xls(.,file=paste0(path_bs_folder,"/working/table 1A.xlsx"))


ngal_clean %>% 
  compareGroups(sepsis_2cat ~ age + d_male +
                  d_dm + d_htn + d_copd + d_cad +
                  d_steroid + d_immunocomp +
                  fever + d_respi + d_uti +
                  d_gi + d_cns + d_softtiss +
                  d_source +
                  d_rigors + d_myalgia + 
                  
                  pulse + d_pulse_gt90 +
                  sysbp + d_sysbp_lt90 +
                  diabp +
                  
                  rr + d_rr_gt22 +
                  gcs + d_gcs_lt15 + 
                  sirs_score + d_sirs_score_gt3 +
                  qsofa + d_qsofa_gt2 + d_periferies_cold +
                  
                  tc_10e3 + d_tc_gt12k + 
                  dc + d_dc_gt80 +
                  plts_10e3 + d_plts_lt10k + 
                  creatine + d_creatine_gt2 + 
                  lactate + d_lactate_gt2 +
                  sodium + ph +
                  tb + d_tb_gt2 +
                  sgot + d_sgot_gt40 +
                  sgpt + d_sgpt_gt41 +
                  ngal,
                data=.,
                method =  c(1,3,
                            3,3,3,3,
                            3,3,
                            2,3,3,
                            3,3,3,
                            3,
                            3,3,
                            
                            1,3,
                            1,3,
                            1,
                            
                            2,3,
                            2,3,
                            2,3,
                            2,3,3,
                            
                            1,3,
                            1,3,
                            1,3,
                            2,3,
                            2,3,
                            1,1,
                            2,3,
                            1,3,
                            1,3,
                            1
                )) %>% 
  createTable(digits = 1,show.all = TRUE,show.n = TRUE,
              q.type = c(2,2),type = 1,sd.type = 2) %>% 
  export2xls(.,file=paste0(path_bs_folder,"/working/table 1B.xlsx"))