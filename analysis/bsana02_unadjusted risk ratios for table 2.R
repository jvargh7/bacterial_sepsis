ngal_clean <- readRDS(paste0(path_bs_folder,"/working/ngal_clean.RDS")) %>% 
  mutate(d_sepsis_2cat = case_when(sepsis_2cat == "Positive" ~ 1,
                                   sepsis_2cat == "Negative" ~ 0,
                                   TRUE ~ NA_real_),
         ngal100 = ngal/100)

# https://stats.stackexchange.com/questions/520662/how-to-add-robust-error-variances-in-glm-poisson-model-in-r
library("sandwich")
library("lmtest")


iv <- c("age", "d_male",
        "d_dm", "d_htn", "d_copd", "d_cad",
        "d_steroid", "d_immunocomp",
        "fever", "d_respi", "d_uti",
        "d_gi", "d_cns", "d_softtiss",
        "d_source",
        "d_rigors", "d_myalgia",
        "pulse", "d_pulse_gt90",
        "sysbp", "d_sysbp_lt90",
        "diabp",
        "rr", "d_rr_gt22",
        "gcs", "d_gcs_lt15",
        "sirs_score", "d_sirs_score_gt3",
        "qsofa", "d_qsofa_gt2", "d_periferies_cold",
        "tc_10e3", "d_tc_gt12k",
        "dc", "d_dc_gt80",
        "plts_10e3", "d_plts_lt10k",
        "creatine", "d_creatine_gt2",
        "lactate", "d_lactate_gt2",
        "sodium", "ph",
        "tb", "d_tb_gt2",
        "sgot", "d_sgot_gt40",
        "sgpt", "d_sgpt_gt41",
        "ngal100")

unadj_rr <- map_dfr(iv,
                    function(x){
                      unadj_model <- glm(paste0("d_sepsis_2cat ~ ",x) %>% 
                                           as.formula(.),
                                         data=ngal_clean,family = poisson())  %>% 
                        coeftest(., vcov = sandwich) %>% 
                        broom::tidy() %>% 
                        mutate(iv = x,
                               coef = round(exp(estimate),2),
                               lci = round(exp(estimate - 1.96*std.error),2),
                               uci = round(exp(estimate + 1.96*std.error),2))
                    }) %>% 
  dplyr::filter(term != "(Intercept)") %>% 
  mutate(coef_ci = paste0(coef," (",lci,", ",uci,")"))

write_csv(unadj_rr,paste0(path_bs_folder,"/working/table1_crude RR.csv"))


