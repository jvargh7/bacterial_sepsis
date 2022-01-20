
# Box plot of predictions ---------

rf_res <- readRDS(paste0(path_bs_folder,"/working/rf_res.RDS"))
glmnet_rs <- readRDS(paste0(path_bs_folder,"/working/glmnet_rs.RDS"))
last_rf_fit <- readRDS(paste0(path_bs_folder,"/working/last_rf_fit.RDS"))
last_glmnet_fit <- readRDS(paste0(path_bs_folder,"/working/last_glmnet_fit.RDS"))


boxplot_A <- bind_rows(last_glmnet_fit$.predictions[[1]] %>%
                     mutate(model = "GLMNET"),
                   last_rf_fit$.predictions[[1]] %>%
                     mutate(model = "Random Forest")) %>% 
  ggplot(data=.,aes(fill=sepsis_2cat,y=.pred_Positive,x=model)) +
  geom_boxplot(position = "dodge") +
  xlab("") +
  ylab("Positive sepsis (probability)") +
  theme_bw()+
  theme(legend.position = "bottom") +
  scale_fill_manual(name="",values=c("red","darkgreen"))

dotplot_B <- bind_rows(last_glmnet_fit$.predictions[[1]] %>%
                         mutate(model = "GLMNET"),
                       last_rf_fit$.predictions[[1]] %>%
                         mutate(model = "Random Forest")) %>% 
  mutate(event = case_when(sepsis_2cat == "Positive" ~ 1,
                           TRUE ~ 0)) %>% 
  arrange(.pred_Positive) %>% 
  ggplot(data=.,aes(y=event,x=.pred_Positive,col=model)) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0,1)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  geom_smooth(aes(x = .pred_Positive, y = event,group=model), se = F, method = "loess") + 
  # you can use stat_smooth in place of geom_smooth
  geom_abline(linetype = 2,col="grey20") +
  theme_bw() +
  xlab("Predicted probability") +
  ylab("Positive sepsis")+
  theme(legend.position = "bottom") +
  scale_color_manual(name="",values=c("purple","orange"))

library(ggpubr)

ggarrange(boxplot_A,
          dotplot_B,ncol = 2,nrow=1,labels = c("A","B")) %>% 
  ggsave(.,filename=paste0(path_bs_folder,"/figures/all predictors discrimination.png"),width = 8,height=5)
