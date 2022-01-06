ngal_clean <- readRDS(paste0(path_bs_folder,"/working/ngal_clean.RDS")) %>% 
  mutate(d_sepsis_2cat = case_when(sepsis_2cat == "Positive" ~ 1,
                                   sepsis_2cat == "Negative" ~ 0,
                                   TRUE ~ NA_real_),
         ngal100 = ngal/100)


library(ROCit)

m1 <- glm(d_sepsis_2cat ~ ngal100,data=ngal_clean,family = poisson())

pred_m1 <- predict(m1,ngal_clean,type = "response")

ROCit_obj <- rocit(score=pred_m1,class=ngal_clean$d_sepsis_2cat)
plot(ROCit_obj)



# log(Pr[Sepsis]) = b0 + b1.NGAL100
# {log(Pr[Sepsis]) - b0}/b1

cutoff <- ROCit_obj$Cutoff[which.max(with(ROCit_obj,TPR - FPR))]
ngal_cutoff = 100*(log(cutoff) - coef(m1)[1])/coef(m1)[2]
Sensitivity <- ROCit_obj$TPR[which.max(with(ROCit_obj,TPR - FPR))]
Specificity <- 1- ROCit_obj$FPR[which.max(with(ROCit_obj,TPR - FPR))]


roc_A <-
  ggplot(data.frame(TPR = ROCit_obj$TPR,FPR = ROCit_obj$FPR),
         aes(x=FPR,y = TPR)) +
  geom_path() +
  geom_abline(slope = 1,intercept=0) +
  xlab("1 - Specificity") +
  ylab("Sensitivity") +
  theme_bw() +
  geom_point(y = Sensitivity,x=(1-Specificity),shape=15)

density_B <- ggplot(data=ngal_clean,
       aes(x=ngal,fill=sepsis_2cat)) +
  geom_density(alpha=0.4) +
  theme_bw() +
  geom_vline(xintercept = ngal_cutoff,col="black",linetype = 2) +
  scale_fill_manual(name = "",values=c("darkgreen","red")) +
  theme(legend.position = "bottom") +
  ylab("Density") +
  xlab("NGAL (mg/dL)")

library(ggpubr)

ggarrange(roc_A,
          density_B,
          nrow=2,
          ncol=1,labels = c("A","B"))

# RR at cut-off ---------

library("sandwich")
library("lmtest")

ngal_clean %>% 
  mutate(ngal_binary = case_when(ngal100 > ngal_cutoff/100 ~ 1,
                                 ngal100 <= ngal_cutoff/100 ~ 0,
                                 TRUE ~ NA_real_)) %>% 
glm(d_sepsis_2cat ~ ngal_binary,data=.,family = poisson()) %>% 
  coeftest(., vcov = sandwich) %>% 
  broom::tidy() %>% 
  mutate(coef = round(exp(estimate),2),
         lci = round(exp(estimate - 1.96*std.error),2),
         uci = round(exp(estimate + 1.96*std.error),2))
