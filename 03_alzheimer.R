#Loading all functions
lapply(list.files(path = "./functions", full.names= T), source)

#Loading all libraries and the Alzheimer disease data set and setting a seed
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
library(ggplot2)
library(dplyr)
library(tidyverse)
set.seed(1807)

#Preprocessing
data_alz <- cbind(diagnosis, predictors) %>% rename(y = diagnosis)
data_alz$y <- as.factor(data_alz$y)
data_alz$male <- as.factor(data_alz$male)

#Applyying shadowvimp with preselection; note that the num.threads parameter (the number of cores the ranger package
#can use for multithreading may need adjusting on your system. respect.unordered.factors is passed to the ranger function
#so categegorical variables are properly used in building the trees)
alz_three_steps <- vim_perm_sim_wrapper(nsims= c(30, 120, 1000),
                                        alphas = c(0.3, 0.15, 0.05),
                                        entire_data = data_alz,
                                        num.threads = 20,
                                        save_vim_history = 'everything',
                                        respect.unordered.factors = "order",
                                        y = y)

#save the result
saveRDS(alz_three_steps, "results/intermediate_results/alzheimer.rds")


#load again
alz_three_steps <- readRDS("results/intermediate_results/alzheimer.rds")

#look at some results
alz_three_steps[[3]]$vimpermsim$test_results$per_variable[["0_05"]] %>% View()
alz_three_steps[[3]][["vimpermsim"]][["test_results"]][["with_correction"]][["pooled"]][["0_05"]] %>% View()

named_pvals <- setNames(alz_three_steps[[3]][["vimpermsim"]][["test_results"]][["with_correction"]][["pooled"]][["0_05"]][,2], 
                        alz_three_steps[[3]][["vimpermsim"]][["test_results"]][["with_correction"]][["pooled"]][["0_05"]][,c(1)]) 
sum(stats::p.adjust(1-named_pvals, method = 'BH', n = 130) <= 0.05)
sum(stats::p.adjust(1-named_pvals, method = 'holm', n = 130) <= 0.05)
sum(1-named_pvals <= 0.05)

#Plotting importances and result of shadowVIMP
plot_vimps <- function(wrapper_object, pooled = T, ...) {
max_step <- wrapper_object %>% length()

decisions <- wrapper_object[[max_step]][["vimpermsim"]][["test_results"]][["with_correction"]][[ifelse(pooled, "pooled", "per_variable")]][["0_05"]] %>% select(c("varname", "Type1_confirmed","FDR_confirmed","FWER_confirmed"))  %>%
  mutate(decision = factor(x = case_when( (FWER_confirmed == 1)&(FDR_confirmed == 1)&(Type1_confirmed == 1)~ "FWER conf.",
                               (FWER_confirmed == 0)&(FDR_confirmed == 1)&(Type1_confirmed == 1)~ "FDR conf.",
                               (FWER_confirmed == 0)&(FDR_confirmed == 0)&(Type1_confirmed == 1)~ "Unadjusted conf.",
                               (FWER_confirmed == 0)&(FDR_confirmed == 0)&(Type1_confirmed == 0)~ "Not significant"
  ), levels = c("FWER conf.", "FDR conf.", "Unadjusted conf.", "Not significant"), ordered = T))%>%
  select(-c( "Type1_confirmed","FDR_confirmed","FWER_confirmed"))



vimps <- wrapper_object[[max_step]][["vimpermsim"]][["vim_simulated"]] %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "varname", values_to = "VIMP") %>%
  filter(!grepl('_permuted', varname)) %>% left_join(decisions, by="varname")


ggplot(vimps, aes(y=reorder(varname, VIMP, FUN = median), x = VIMP, fill= decision), ...) + geom_boxplot(outlier.size = 0.5)  +
  scale_y_discrete(labels = function(labels) str_replace_all(labels, "_", " ")) + theme_bw() + theme(...)

}


#Producing plots (Figure 2)
plot_vimps(alz_three_steps, axis.title=element_blank(), legend.position = "none", text=element_text(size=20)) %>% 
  ggsave(filename = 'results/figures/figure_2_alzheimer_plot_shadowVIMP.png', width = 1300, height = 1800, dpi = 72, unit = 'px')



#Producing base RF VIMP plot as displayed in introduction (Figure 1)
set.seed(1807)
base_rf <- ranger::ranger(formula = y~., data=data_alz, num.trees=10000, importance="permutation", scale.permutation.importance = T)
vimps_baserf <- data.frame(VIMP = base_rf$variable.importance, varname = base_rf$variable.importance %>% names()) %>% arrange(desc(VIMP))
(ggplot(data = vimps_baserf[1:35,], aes(x=VIMP, y=reorder(varname, VIMP))) + geom_col() +theme_bw()+
  theme(axis.title=element_blank(), text=element_text(size=15), legend.position ="none")) %>% 
  ggsave(filename = 'results/figures/figure_1_alzheimer_plot_baseRF.png', width = 1000, height = 1200, dpi = 72, unit = 'px')


writeLines(capture.output(sessionInfo()), "results/logs/sessionInfo_03_alzheimer.txt")
