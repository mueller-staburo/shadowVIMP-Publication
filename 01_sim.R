setwd("/nfsmb/koll/hornung/Projects/SideProjects/RFVarsel/Code_GitHub/shadowVIMP-Publication")

Sys.setenv(OPENBLAS_NUM_THREADS = 2)
Sys.getenv("OPENBLAS_NUM_THREADS")
Sys.setenv(OMP_NUM_THREADS = 1)
Sys.getenv("OMP_NUM_THREADS")
data.table::setDTthreads(1)


# Action required! Specify the number of parallel operations based on your system's capabilities (the 100 pre-specified here may be too much order
# too few):

num_cores <- 100



lapply(list.files(path = "./functions/", full.names= T), source)
set.seed(1807)
seed_list <- floor(runif(1000, min = 1, max = 999999))

### Machine setup
# Please choose appropriate values
num.threads.ranger <- 1    #ranger multithreading
num.replicates <- 100        #how many replicates of the same design?

boruta_maxRuns <- 1200


#DEG 50
evaluatesetting1 <- function(i) {
  
  require("dplyr")
  require("ranger")
  
  set.seed(seed_list[[i]])
  data <- simulation.data.cor(100, rep(50, 6),  5000)
  vim_perm_sim_wrapper(data, alphas=c(0.05), nsims=c(1000), 
                       additional_alphas = c(0.01, 0.1),
                       additional_iter_prop = seq(0.2, 0.8, by=0.2), 
                       save_vim_history = "none", num.threads = num.threads.ranger)
  
}

evaluatesetting2 <- function(i) {
  
  require("dplyr")
  require("ranger")
  
  set.seed(seed_list[[i]])
  data <- simulation.data.cor(100, rep(50, 6),  5000)
  vim_perm_sim_wrapper(data, alphas=c(0.3, 0.15, 0.05), nsims=c(30, 120, 1000), 
                       additional_alphas = c(0.01, 0.1),
                       additional_iter_prop = seq(0.2, 0.8, by=0.2), 
                       save_vim_history = "none", num.threads = num.threads.ranger)
}

evaluatesetting3 <- function(i) {
  
  require("dplyr")
  require("ranger")
  require("Boruta")
  
  boruta_maxRuns <- 1200
  
  set.seed(seed_list[[i]])
  data <- simulation.data.cor(100, rep(50, 6),  5000)
  boruta_wrapper(data, formula=y~., num.trees = 10000, maxRuns = boruta_maxRuns, num.threads = num.threads.ranger, remove_vim_history = ifelse(i == 50, F, T))
}

evaluatesetting4 <- function(i) {
  
  require("dplyr")
  require("ranger")
  require("Boruta")
  
  boruta_maxRuns <- 1200
  
  set.seed(seed_list[[i]])
  data <- simulation.data.cor(100, rep(50, 6),  5000)
  boruta_wrapper(data, formula=y~., num.trees = 500, maxRuns = boruta_maxRuns, num.threads = num.threads.ranger, remove_vim_history = ifelse(i == 50, F, T))
}

evaluatesetting5 <- function(i) {
  
  require("dplyr")
  require("ranger")
  
  set.seed(seed_list[[i]])
  data <- simulation.data.cor(100, rep(50, 6),  5000)
  ranger::importance_pvalues(x = ranger::holdoutRF(data=data, formula = y~., num.trees=10000, num.threads=num.threads.ranger),
                             method = "janitza") %>% 
    as.data.frame() %>% 
    mutate("BH" = stats::p.adjust(pvalue, method = "BH"),
           "Holm" = stats::p.adjust(pvalue, method = "holm"))
}


#FRIED
evaluatesetting6 <- function(i) {
  
  require("dplyr")
  require("ranger")
  
  set.seed(seed_list[[i]])
  data <- data.frame(mlbench::mlbench.friedman1(n = 100))
  vim_perm_sim_wrapper(data, alphas=c(0.05), nsims=c(1000), 
                       additional_alphas = c(0.01, 0.1),
                       additional_iter_prop = seq(0.2, 0.8, by=0.2), 
                       save_vim_history = "none", num.threads = num.threads.ranger)
}

evaluatesetting7 <- function(i) {
  
  require("dplyr")
  require("ranger")
  
  set.seed(seed_list[[i]])
  data <- data.frame(mlbench::mlbench.friedman1(n = 100))
  vim_perm_sim_wrapper(data, alphas=c(0.3, 0.15, 0.05), nsims=c(30, 120, 1000), 
                       additional_alphas = c(0.01, 0.1),
                       additional_iter_prop = seq(0.2, 0.8, by=0.2), 
                       save_vim_history = "none", num.threads = num.threads.ranger)
}

evaluatesetting8 <- function(i) {
  
  require("dplyr")
  require("ranger")
  require("Boruta")
  
  boruta_maxRuns <- 1200
  
  set.seed(seed_list[[i]])
  data <- data.frame(mlbench::mlbench.friedman1(n = 100))
  boruta_wrapper(data, formula=y~., num.trees = 10000, maxRuns = boruta_maxRuns, num.threads = num.threads.ranger, remove_vim_history = ifelse(i == 50, F, T))
}

evaluatesetting9 <- function(i) {
  
  require("dplyr")
  require("ranger")
  require("Boruta")
  
  boruta_maxRuns <- 1200
  
  set.seed(seed_list[[i]])
  data <- data.frame(mlbench::mlbench.friedman1(n = 100))
  boruta_wrapper(data, formula=y~., num.trees = 500, maxRuns = boruta_maxRuns, num.threads = num.threads.ranger, remove_vim_history = ifelse(i == 50, F, T))
}

evaluatesetting10 <- function(i) {
  
  require("dplyr")
  require("ranger")
  
  set.seed(seed_list[[i]])
  data <- data.frame(mlbench::mlbench.friedman1(n = 100))
  rfvimptest2(data=data, yname = "y", type = 'pval', num.threads = num.threads.ranger, ntree=10000)
}

evaluatesetting11 <- function(i) {
  
  require("dplyr")
  require("ranger")
  
  set.seed(seed_list[[i]])
  data <- data.frame(mlbench::mlbench.friedman1(n = 100))
  rfvimptest2(data=data, yname = 'y', type = 'pval', num.threads = num.threads.ranger, ntree=500)
}


#DEG 10

##proposed
evaluatesetting12 <- function(i) {
  
  require("dplyr")
  require("ranger")
  
  set.seed(seed_list[[i]])
  data <- simulation.data.cor(100, rep(10, 6),  5000)
  vim_perm_sim_wrapper(data, alphas=c(0.05), nsims=c(1000), 
                       additional_alphas = c(0.01, 0.1),
                       additional_iter_prop = seq(0.2, 0.8, by=0.2), 
                       save_vim_history = "none", num.threads = num.threads.ranger)
  
}

evaluatesetting13 <- function(i) {
  
  require("dplyr")
  require("ranger")
  
  set.seed(seed_list[[i]])
  data <- simulation.data.cor(100, rep(10, 6),  5000)
  vim_perm_sim_wrapper(data, alphas=c(0.3, 0.15, 0.05), nsims=c(30, 120, 1000), 
                       additional_alphas = c(0.01, 0.1),
                       additional_iter_prop = seq(0.2, 0.8, by=0.2), 
                       save_vim_history = "none", num.threads = num.threads.ranger)
}

##boruta
evaluatesetting14 <- function(i) {
  
  require("dplyr")
  require("ranger")
  require("Boruta")
  
  
  set.seed(seed_list[[i]])
  data <- simulation.data.cor(100, rep(10, 6),  5000)
  boruta_wrapper(data, formula=y~., num.trees = 10000, maxRuns = boruta_maxRuns, num.threads = num.threads.ranger, remove_vim_history = T)
}

evaluatesetting15 <- function(i) {
  
  require("dplyr")
  require("ranger")
  require("Boruta")
  
  
  set.seed(seed_list[[i]])
  data <- simulation.data.cor(100, rep(10, 6),  5000)
  boruta_wrapper(data, formula=y~., num.trees = 500, maxRuns = boruta_maxRuns, num.threads = num.threads.ranger, remove_vim_history = T)
}

##janitza
evaluatesetting16 <- function(i) {
  
  require("dplyr")
  require("ranger")
  
  set.seed(seed_list[[i]])
  data <- simulation.data.cor(100, rep(10, 6),  5000)
  ranger::importance_pvalues(x = ranger::holdoutRF(data=data, formula = y~., num.trees=10000, num.threads=num.threads.ranger),
                             method = "janitza") %>% 
    as.data.frame() %>% 
    mutate("BH" = stats::p.adjust(pvalue, method = "BH"),
           "Holm" = stats::p.adjust(pvalue, method = "holm"))
}


#strobl
evaluatesetting17 <- function(i) {
  
  require("dplyr")
  require("ranger")
  
  set.seed(seed_list[[i]])
  data <- data.frame(strobl_sim(relevance = 0.2) %>% mutate(y=as.factor(y)))
  vim_perm_sim_wrapper(data, alphas=c(0.05), nsims=c(1000), 
                       additional_alphas = c(0.01, 0.1),
                       additional_iter_prop = seq(0.2, 0.8, by=0.2), 
                       save_vim_history = "none", num.threads = num.threads.ranger,
                       respect.unordered.factors = "order")
}

evaluatesetting18 <- function(i) {
  
  require("dplyr")
  require("ranger")
  
  set.seed(seed_list[[i]])
  data <- data.frame(strobl_sim(relevance = 0.2) %>% mutate(y=as.factor(y)))
  vim_perm_sim_wrapper(data, alphas=c(0.3, 0.15, 0.05), nsims=c(30, 120, 1000), 
                       additional_alphas = c(0.01, 0.1),
                       additional_iter_prop = seq(0.2, 0.8, by=0.2), 
                       save_vim_history = "none", num.threads = num.threads.ranger,
                       respect.unordered.factors = "order")
}

##boruta
evaluatesetting19 <- function(i) {
  
  require("dplyr")
  require("ranger")
  require("Boruta")
  
  
  set.seed(seed_list[[i]])
  data <- data.frame(strobl_sim(relevance = 0.2) %>% mutate(y=as.factor(y)))
  boruta_wrapper(data, formula=y~., num.trees = 10000, maxRuns = boruta_maxRuns, num.threads = num.threads.ranger, remove_vim_history = T,
                 respect.unordered.factors = "order")
}

evaluatesetting20 <- function(i) {
  
  require("dplyr")
  require("ranger")
  require("Boruta")
  
  
  set.seed(seed_list[[i]])
  data <- data.frame(strobl_sim(relevance = 0.2) %>% mutate(y=as.factor(y)))
  boruta_wrapper(data, formula=y~., num.trees = 500, maxRuns = boruta_maxRuns, num.threads = num.threads.ranger, remove_vim_history = T,
                 respect.unordered.factors = "order")
}

##rfvimptest
evaluatesetting21 <- function(i) {
  
  require("dplyr")
  require("ranger")
  
  set.seed(seed_list[[i]])
  data <- data.frame(strobl_sim(relevance = 0.2) %>% mutate(y=as.factor(y)))
  rfvimptest2(data=data, yname = "y", type='pval', ntree=10000, num.threads = num.threads.ranger,
              respect.unordered.factors = "order")
}

evaluatesetting22 <- function(i) {
  
  require("dplyr")
  require("ranger")
  
  set.seed(seed_list[[i]])
  data <- data.frame(strobl_sim(relevance = 0.2) %>% mutate(y=as.factor(y)))
  rfvimptest2(data=data, yname = "y", type='pval', ntree=500, num.threads = num.threads.ranger,
              respect.unordered.factors = "order")
}

#nicodemus (null case)
evaluatesetting23 <- function(i) {
  
  require("dplyr")
  require("ranger")
  
  set.seed(seed_list[[i]])
  data <- nicodemeus_sim()
  data$y <- data$y %>% sample()
  vim_perm_sim_wrapper(data, alphas=c(0.05), nsims=c(1000), 
                       additional_alphas = c(0.01, 0.1),
                       additional_iter_prop = seq(0.2, 0.8, by=0.2), 
                       save_vim_history = "none", num.threads = num.threads.ranger)
}

evaluatesetting24 <- function(i) {
  
  require("dplyr")
  require("ranger")
  
  set.seed(seed_list[[i]])
  data <- nicodemeus_sim()
  data$y <- data$y %>% sample()
  vim_perm_sim_wrapper(data, alphas=c(0.3, 0.15, 0.05), nsims=c(30, 120, 1000), 
                       additional_alphas = c(0.01, 0.1),
                       additional_iter_prop = seq(0.2, 0.8, by=0.2), 
                       save_vim_history = "none", num.threads = num.threads.ranger)
}

##boruta
evaluatesetting25 <- function(i) {
  
  require("dplyr")
  require("ranger")
  require("Boruta")
  
  
  set.seed(seed_list[[i]])
  data <- nicodemeus_sim()
  data$y <- data$y %>% sample()
  boruta_wrapper(data, formula=y~., num.trees = 10000, maxRuns = boruta_maxRuns, num.threads = num.threads.ranger, remove_vim_history = T)
}

evaluatesetting26 <- function(i) {
  
  require("dplyr")
  require("ranger")
  require("Boruta")
  
  
  set.seed(seed_list[[i]])
  data <- nicodemeus_sim()
  data$y <- data$y %>% sample()
  boruta_wrapper(data, formula=y~., num.trees = 500, maxRuns = boruta_maxRuns, num.threads = num.threads.ranger, remove_vim_history = T)
}

##rfvimptest
evaluatesetting27 <- function(i) {
  
  require("dplyr")
  require("ranger")
  
  set.seed(seed_list[[i]])
  data <- nicodemeus_sim()
  data$y <- data$y %>% sample()
  rfvimptest2(data=data, yname = 'y', type = 'pval', ntree=10000, num.threads = num.threads.ranger)
}

evaluatesetting28 <- function(i) {
  
  require("dplyr")
  require("ranger")
  
  set.seed(seed_list[[i]])
  data <- nicodemeus_sim()
  data$y <- data$y %>% sample()
  rfvimptest2(data=data, yname = 'y', type = 'pval', ntree=500, num.threads = num.threads.ranger)
}


# Load libraries necessary for parallelization:

library(parallel)
library(doParallel)


# Start the cluster:

cl <- makeCluster(num_cores, type = "PSOCK")


# Register the parallel backend:

registerDoParallel(cl)


# Export the objects in the workspace to the
# parallel jobs:

clusterExport(cl, varlist = ls())



# Design from Degenhardt et al. 2019 with group size 50

cat("Starting with Proposed method ... \n")
proposed_without_preselect_deg_50 <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting1(z)}))

causal <- add_degenhardt_informative_status(group_size = 50)
proposed_without_preselect_deg_50_small  <- try(publication_results_proposed_without_preselect(proposed_without_preselect_deg_50,
                                                                                               alpha=0.05,
                                                                                               true_informatives = causal,
                                                                                               total_no_vars = 5000))
#savings
save_RDS_and_delete_object(proposed_without_preselect_deg_50_small, subfolder = "./code_rfvarsel/deg50")
save_RDS_and_delete_object(proposed_without_preselect_deg_50, subfolder = "./code_rfvarsel/deg50")
rm(causal)


index <- 1
save(index, file=paste0("index", index, ".Rda"))

### with pre-selection 
proposed_with_preselect_deg_50 <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting2(z)}))

#small
causal <- add_degenhardt_informative_status(group_size = 50)
proposed_with_preselect_deg_50_small <- try(publication_results_proposed_with_preselect(proposed_with_preselect_deg_50,
                                                                                        alpha=0.05,
                                                                                        true_informatives = causal,
                                                                                        total_no_vars = 5000))

#savings
save_RDS_and_delete_object(proposed_with_preselect_deg_50_small, subfolder = "./code_rfvarsel/deg50")
save_RDS_and_delete_object(proposed_with_preselect_deg_50, subfolder = "./code_rfvarsel/deg50")
rm(causal)


index <- index + 1
save(index, file=paste0("index", index, ".Rda"))


## Boruta
cat("Starting with Boruta ... \n")

### with num.trees = 10000
boruta10000_deg_50 <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting3(z)}))

#small
causal <- add_degenhardt_informative_status(group_size = 50)
boruta10000_deg_50_small <- try(publication_results_boruta(boruta10000_deg_50,
                                                           0.05,
                                                           true_informatives = causal,
                                                           total_no_vars = 5000))

#saving
save_RDS_and_delete_object(boruta10000_deg_50_small, subfolder = "./code_rfvarsel/deg50")
save_RDS_and_delete_object(boruta10000_deg_50, subfolder = "./code_rfvarsel/deg50")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))


### with num.trees = 500 (the default)
boruta500_deg_50 <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting4(z)}))

#small
causal <- add_degenhardt_informative_status(group_size = 50)
boruta500_deg_50_small <- try(publication_results_boruta(boruta500_deg_50,
                                                         0.05,
                                                         true_informatives = causal,
                                                         total_no_vars = 5000))
#savings
save_RDS_and_delete_object(boruta500_deg_50_small, subfolder = "./code_rfvarsel/deg50")
save_RDS_and_delete_object(boruta500_deg_50, subfolder = "./code_rfvarsel/deg50")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))



## Vita
cat("Starting with vita ... \n")
vita_deg_50 <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting5(z)}))

#small
causal <- add_degenhardt_informative_status(group_size = 50)
vita_deg_50_small <- try(publication_results_vita(vita_deg_50,
                                                  0.05,
                                                  true_informatives = causal,
                                                  total_no_vars = 5000))
#savings
save_RDS_and_delete_object(vita_deg_50_small, subfolder = "./code_rfvarsel/deg50")
save_RDS_and_delete_object(vita_deg_50, subfolder = "./code_rfvarsel/deg50")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))



#degenhardt group size 10


cat("Starting with Proposed method ... \n")
#parallel
proposed_without_preselect_deg_10 <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting12(z)}))

#small
causal <- add_degenhardt_informative_status(group_size = 10)
proposed_without_preselect_deg_10_small  <- try(publication_results_proposed_without_preselect(proposed_without_preselect_deg_10,
                                                                                               alpha=0.05,
                                                                                               true_informatives = causal,
                                                                                               total_no_vars = 5000))
#savings
save_RDS_and_delete_object(proposed_without_preselect_deg_10_small, subfolder = "./code_rfvarsel/deg10")
save_RDS_and_delete_object(proposed_without_preselect_deg_10, subfolder = "./code_rfvarsel/deg10")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))


### with pre-selection 
#parallel
proposed_with_preselect_deg_10 <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting13(z)}))

#small
causal <- add_degenhardt_informative_status(group_size = 10)
proposed_with_preselect_deg_10_small <- try(publication_results_proposed_with_preselect(proposed_with_preselect_deg_10,
                                                                                        alpha=0.05,
                                                                                        true_informatives = causal,
                                                                                        total_no_vars = 5000))

#savings
save_RDS_and_delete_object(proposed_with_preselect_deg_10_small, subfolder = "./code_rfvarsel/deg10")
save_RDS_and_delete_object(proposed_with_preselect_deg_10, subfolder = "./code_rfvarsel/deg10")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))


## Boruta
cat("Starting with Boruta ... \n")

### with num.trees = 10000
#parallel
boruta10000_deg_10 <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting14(z)}))

#small
causal <- add_degenhardt_informative_status(group_size = 10)
boruta10000_deg_10_small <- try(publication_results_boruta(boruta10000_deg_10,
                                                           0.05,
                                                           true_informatives = causal,
                                                           total_no_vars = 5000))

#saving
save_RDS_and_delete_object(boruta10000_deg_10_small, subfolder = "./code_rfvarsel/deg10")
save_RDS_and_delete_object(boruta10000_deg_10, subfolder = "./code_rfvarsel/deg10")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))



### with num.trees = 500 (the default)
#parallel
boruta500_deg_10 <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting15(z)}))

#small
causal <- add_degenhardt_informative_status(group_size = 10)
boruta500_deg_10_small <- try(publication_results_boruta(boruta500_deg_10,
                                                         0.05,
                                                         true_informatives = causal,
                                                         total_no_vars = 5000))
#savings
save_RDS_and_delete_object(boruta500_deg_10_small, subfolder = "./code_rfvarsel/deg10")
save_RDS_and_delete_object(boruta500_deg_10, subfolder = "./code_rfvarsel/deg10")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))



## Vita
cat("Starting with vita ... \n")

#parallel
vita_deg_10 <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting16(z)}))

#small
causal <- add_degenhardt_informative_status(group_size = 10)
vita_deg_10_small <- try(publication_results_vita(vita_deg_10,
                                                  0.05,
                                                  true_informatives = causal,
                                                  total_no_vars = 5000))
#savings
save_RDS_and_delete_object(vita_deg_10_small, subfolder = "./code_rfvarsel/deg10")
save_RDS_and_delete_object(vita_deg_10, subfolder = "./code_rfvarsel/deg10")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))






# Design Friedman1


## Proposed method
cat("Starting with Proposed method ... \n")
### without pre-selection
proposed_without_preselect_fried <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting6(z)}))


#small
causal <- add_friedmann_informative_status()
proposed_without_preselect_fried_small <- try(publication_results_proposed_without_preselect(proposed_without_preselect_fried,
                                                                                             0.05,
                                                                                             true_informatives = causal,
                                                                                             total_no_vars = 10))
#savings
save_RDS_and_delete_object(proposed_without_preselect_fried_small, subfolder = "./code_rfvarsel/fried")
save_RDS_and_delete_object(proposed_without_preselect_fried, subfolder = "./code_rfvarsel/fried")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))



### with pre-selection 
proposed_with_preselect_fried <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting7(z)}))

#small
causal <- add_friedmann_informative_status()
proposed_with_preselect_fried_small <- try(publication_results_proposed_with_preselect(proposed_with_preselect_fried,
                                                                                       0.05,
                                                                                       true_informatives = causal,
                                                                                       total_no_vars = 10))
#savings
save_RDS_and_delete_object(proposed_with_preselect_fried_small, subfolder = "./code_rfvarsel/fried")
save_RDS_and_delete_object(proposed_with_preselect_fried, subfolder = "./code_rfvarsel/fried")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))




## Boruta
cat("Starting with Boruta ... \n")
boruta_maxRuns <- 1200

### with num.trees = 10000
boruta10000_fried <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting8(z)}))

#small
causal <- add_friedmann_informative_status()
boruta10000_fried_small <- try(publication_results_boruta(boruta10000_fried,
                                                          0.05,
                                                          true_informatives = causal,
                                                          total_no_vars = 10))
#savings
save_RDS_and_delete_object(boruta10000_fried_small, subfolder = "./code_rfvarsel/fried")
save_RDS_and_delete_object(boruta10000_fried, subfolder = "./code_rfvarsel/fried")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))



### with num.trees = 500 (the default)
boruta500_fried <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting9(z)}))

#small
causal <- add_friedmann_informative_status()
boruta500_fried_small <- try(publication_results_boruta(boruta500_fried,
                                                        0.05,
                                                        true_informatives = causal,
                                                        total_no_vars = 10))
#savings
save_RDS_and_delete_object(boruta500_fried_small, subfolder = "./code_rfvarsel/fried")
save_RDS_and_delete_object(boruta500_fried, subfolder = "./code_rfvarsel/fried")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))




## RFVIMPTEST
cat("Starting with rfvimptest ... \n")
rfvimptest10000_fried <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting10(z)}))

#small
causal <- add_friedmann_informative_status()
rfvimptest10000_fried_small <- try(publication_results_rfvimptest_pvalues(rfvimptest10000_fried,
                                                                  0.05,
                                                                  true_informatives = causal,
                                                                  total_no_vars = 10))
#savings
save_RDS_and_delete_object(rfvimptest10000_fried_small, subfolder = "./code_rfvarsel/fried")
save_RDS_and_delete_object(rfvimptest10000_fried, subfolder = "./code_rfvarsel/fried")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))




rfvimptest500_fried <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting11(z)}))

#small
causal <- add_friedmann_informative_status()
rfvimptest500_fried_small <- try(publication_results_rfvimptest_pvalues(rfvimptest500_fried,
                                                                0.05,
                                                                true_informatives = causal,
                                                                total_no_vars = 10))
#savings
save_RDS_and_delete_object(rfvimptest500_fried_small, subfolder = "./code_rfvarsel/fried")
save_RDS_and_delete_object(rfvimptest500_fried, subfolder = "./code_rfvarsel/fried")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))



# Design Strobl


## Proposed method
cat("Starting with Proposed method ... \n")
### without pre-selection

#parallel
proposed_without_preselect_strobl <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting17(z)}))

#small
causal <- add_strobl_informative_status()
proposed_without_preselect_strobl_small <- try(publication_results_proposed_without_preselect(proposed_without_preselect_strobl,
                                                                                              alpha = 0.05,
                                                                                              true_informatives = causal,
                                                                                              total_no_vars = 5))

#savings
save_RDS_and_delete_object(proposed_without_preselect_strobl_small, subfolder = "./code_rfvarsel/strobl")
save_RDS_and_delete_object(proposed_without_preselect_strobl, subfolder = "./code_rfvarsel/strobl")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))



### with pre-selection 

#parallel
proposed_with_preselect_strobl <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting18(z)}))

#small
causal <- add_strobl_informative_status()
proposed_with_preselect_strobl_small <- try(publication_results_proposed_with_preselect(proposed_with_preselect_strobl,
                                                                                        alpha = 0.05,
                                                                                        true_informatives = causal,
                                                                                        total_no_vars = 5))

#savings
save_RDS_and_delete_object(proposed_with_preselect_strobl_small, subfolder = "./code_rfvarsel/strobl")
save_RDS_and_delete_object(proposed_with_preselect_strobl, subfolder = "./code_rfvarsel/strobl")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))



## Boruta
cat("Starting with Boruta ... \n")

### with num.trees = 10000

#parallel
boruta10000_strobl <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting19(z)}))

#small
causal <- add_strobl_informative_status()
boruta10000_strobl_small <- try(publication_results_boruta(boruta10000_strobl,
                                                           alpha = 0.05,
                                                           true_informatives = causal,
                                                           total_no_vars = 5))

#savings
save_RDS_and_delete_object(boruta10000_strobl_small, subfolder = "./code_rfvarsel/strobl")
save_RDS_and_delete_object(boruta10000_strobl, subfolder = "./code_rfvarsel/strobl")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))



#parallel
boruta500_strobl <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting20(z)}))

#small
causal <- add_strobl_informative_status()
boruta500_strobl_small <- try(publication_results_boruta(boruta500_strobl,
                                                         alpha = 0.05,
                                                         true_informatives = causal,
                                                         total_no_vars = 5))

#savings
save_RDS_and_delete_object(boruta500_strobl_small, subfolder = "./code_rfvarsel/strobl")
save_RDS_and_delete_object(boruta500_strobl, subfolder = "./code_rfvarsel/strobl")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))



## RFVIMPTEST
cat("Starting with rfvimptest ... \n")

#10000 trees

#parallel
rfvimptest10000_strobl <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting21(z)}))

#small
causal <- add_strobl_informative_status()
rfvimptest10000_strobl_small <- try(publication_results_rfvimptest_pvalues(rfvimptest10000_strobl,
                                                                   alpha = 0.05,
                                                                   true_informatives = causal,
                                                                   total_no_vars = 5))

#savings
save_RDS_and_delete_object(rfvimptest10000_strobl_small, subfolder = "./code_rfvarsel/strobl")
save_RDS_and_delete_object(rfvimptest10000_strobl, subfolder = "./code_rfvarsel/strobl")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))




#500 trees

#parallel
rfvimptest500_strobl <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting22(z)}))

#small
causal <- add_strobl_informative_status()
rfvimptest500_strobl_small <- try(publication_results_rfvimptest_pvalues(rfvimptest500_strobl,
                                                                 alpha = 0.05,
                                                                 true_informatives = causal,
                                                                 total_no_vars = 5))

#savings
save_RDS_and_delete_object(rfvimptest500_strobl_small, subfolder = "./code_rfvarsel/strobl")
save_RDS_and_delete_object(rfvimptest500_strobl, subfolder = "./code_rfvarsel/strobl")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))



# Design Nicodemus


## Proposed method
cat("Starting with Proposed method ... \n")
### without pre-selection

#parallel
proposed_without_preselect_nicodemus <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting23(z)}))

#small
causal <- add_nicodemeus_informative_status()
proposed_without_preselect_nicodemus_small <- try(publication_results_proposed_without_preselect(proposed_without_preselect_nicodemus,
                                                                                                 alpha=0.05,
                                                                                                 true_informatives=causal,
                                                                                                 total_no_vars = 12))


#savings
save_RDS_and_delete_object(proposed_without_preselect_nicodemus_small, subfolder = "./code_rfvarsel/nicodemus")
save_RDS_and_delete_object(proposed_without_preselect_nicodemus, subfolder = "./code_rfvarsel/nicodemus")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))


### with pre-selection 

#parallel
proposed_with_preselect_nicodemus <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting24(z)}))

#small
causal <- add_nicodemeus_informative_status()
proposed_with_preselect_nicodemus_small <- try(publication_results_proposed_with_preselect(proposed_with_preselect_nicodemus,
                                                                                           alpha=0.05,
                                                                                           true_informatives=causal,
                                                                                           total_no_vars = 12))
#savings
save_RDS_and_delete_object(proposed_with_preselect_nicodemus_small, subfolder = "./code_rfvarsel/nicodemus")
save_RDS_and_delete_object(proposed_with_preselect_nicodemus, subfolder = "./code_rfvarsel/nicodemus")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))



## Boruta
cat("Starting with Boruta ... \n")


### with num.trees = 10000
#parallel
boruta10000_nicodemus <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting25(z)}))

#small
causal <- add_nicodemeus_informative_status()
boruta10000_nicodemus_small <- try(publication_results_boruta(boruta10000_nicodemus,
                                                              alpha=0.05,
                                                              true_informatives=causal,
                                                              total_no_vars = 12))
#savings
save_RDS_and_delete_object(boruta10000_nicodemus_small, subfolder = "./code_rfvarsel/nicodemus")
save_RDS_and_delete_object(boruta10000_nicodemus, subfolder = "./code_rfvarsel/nicodemus")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))


### with num.trees = 500 (the default)

#parallel
boruta500_nicodemus <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting26(z)}))

#small
causal <- add_nicodemeus_informative_status()
boruta500_nicodemus_small <- try(publication_results_boruta(boruta500_nicodemus,
                                                            alpha=0.05,
                                                            true_informatives=causal,
                                                            total_no_vars = 12))

#savings
save_RDS_and_delete_object(boruta500_nicodemus_small, subfolder = "./code_rfvarsel/nicodemus")
save_RDS_and_delete_object(boruta500_nicodemus, subfolder = "./code_rfvarsel/nicodemus")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))



## RFVIMPTEST
cat("Starting with rfvimptest ... \n")
#10000 trees
#parallel
rfvimptest10000_nicodemus <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting27(z)}))

#small
causal <- add_nicodemeus_informative_status()
rfvimptest10000_nicodemus_small <- try(publication_results_rfvimptest_pvalues(rfvimptest10000_nicodemus,
                                                                      alpha=0.05,
                                                                      true_informatives=causal,
                                                                      total_no_vars = 12))
#savings
save_RDS_and_delete_object(rfvimptest10000_nicodemus_small, subfolder = "./code_rfvarsel/nicodemus")
save_RDS_and_delete_object(rfvimptest10000_nicodemus, subfolder = "./code_rfvarsel/nicodemus")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))


#500 trees

#parallel
rfvimptest500_nicodemus <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting28(z)}))

#small
causal <- add_nicodemeus_informative_status()
rfvimptest500_nicodemus_small <- try(publication_results_rfvimptest_pvalues(rfvimptest500_nicodemus,
                                                                    alpha=0.05,
                                                                    true_informatives=causal,
                                                                    total_no_vars = 12))
#savings
save_RDS_and_delete_object(rfvimptest500_nicodemus_small, subfolder = "./code_rfvarsel/nicodemus")
save_RDS_and_delete_object(rfvimptest500_nicodemus, subfolder = "./code_rfvarsel/nicodemus")
rm(causal)

index <- index + 1
save(index, file=paste0("index", index, ".Rda"))


sessionInfo()


# Stop the cluster:

stopCluster(cl)

