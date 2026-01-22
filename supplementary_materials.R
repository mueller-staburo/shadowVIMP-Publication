# Additional simulations to answer reviewer's comments
# Goal: check the influence of column-wise permutation in shadowVIMP 
# (in comparison to proposed row-wise)
num_cores <- 20

# Read custom functions and generate seed
lapply(list.files(path = "./functions/", full.names= T), source)
set.seed(1807)
seed_list <- floor(runif(1000, min = 1, max = 999999))

# Simulation designs ----
## Degenhardt et al.(2019) with a group size of 50 ----

# Method: shadowVIMP without pre-selection
evaluatesetting1_column <- function(i) {
  
  require("dplyr")
  require("ranger")
  
  set.seed(seed_list[[i]])
  data <- simulation.data.cor(100, rep(50, 6),  5000)
  vim_perm_sim_wrapper(data, alphas=c(0.05), 
                       nsims=c(1000), 
                       additional_alphas = c(0.01, 0.1),
                       additional_iter_prop = seq(0.2, 0.8, by=0.2),
                       save_vim_history = "none",
                       num.threads = num.threads.ranger,
                       permute = "columns")

}

# Method: shadowVIMP with pre-selection
evaluatesetting2_column <- function(i) {

  require("dplyr")
  require("ranger")

  set.seed(seed_list[[i]])
  data <- simulation.data.cor(100, rep(50, 6),  5000)
  vim_perm_sim_wrapper(data, alphas=c(0.3, 0.15, 0.05), nsims=c(30, 120, 1000),
                       additional_alphas = c(0.01, 0.1),
                       additional_iter_prop = seq(0.2, 0.8, by=0.2),
                       save_vim_history = "none", num.threads = num.threads.ranger,
                       permute = "columns")
}

# Nicodemus et al. (2010) ----
# Method: shadowVIMP without pre-selection
evaluatesetting23_column <- function(i) {

  require("dplyr")
  require("ranger")

  set.seed(seed_list[[i]])
  data <- nicodemeus_sim()
  data$y <- data$y %>% sample()
  vim_perm_sim_wrapper(data, alphas=c(0.05), nsims=c(1000),
                       additional_alphas = c(0.01, 0.1),
                       additional_iter_prop = seq(0.2, 0.8, by=0.2),
                       save_vim_history = "none",
                       num.threads = num.threads.ranger,
                       permute = "columns")
}

# Method: shadowVIMP with pre-selection
evaluatesetting24_column <- function(i) {

  require("dplyr")
  require("ranger")

  set.seed(seed_list[[i]])
  data <- nicodemeus_sim()
  data$y <- data$y %>% sample()
  vim_perm_sim_wrapper(data, alphas=c(0.3, 0.15, 0.05), nsims=c(30, 120, 1000),
                       additional_alphas = c(0.01, 0.1),
                       additional_iter_prop = seq(0.2, 0.8, by=0.2),
                       save_vim_history = "none", num.threads = num.threads.ranger,
                       permute = "columns")
}

# Machine setup ----
# Please choose appropriate values
num.threads.ranger <- 1    #ranger multithreading
num.replicates <- 100      #how many replicates of the same design?

library(parallel)
library(doParallel)


# Start the cluster:

cl <- makeCluster(num_cores, type = "PSOCK")

# Export the objects in the workspace to the parallel jobs:

clusterExport(cl, varlist = ls())


# Run simulations ----
## Degenhardt et al.(2019) group size 50 ----
### shadowVIMP without pre-selection ----
proposed_without_preselect_deg_50 <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting1_column(z)}))

causal <- add_degenhardt_informative_status(group_size = 50)
proposed_without_preselect_deg_50_small  <- try(publication_results_proposed_without_preselect(proposed_without_preselect_deg_50,
                                                                                               alpha=0.05,
                                                                                               true_informatives = causal,
                                                                                               total_no_vars = 5000))
#savings
save_RDS_and_delete_object(proposed_without_preselect_deg_50_small, subfolder = "./results_supplementary/intermediate_results/deg50")
save_RDS_and_delete_object(proposed_without_preselect_deg_50, subfolder = "./results_supplementary/intermediate_results/deg50")
rm(causal)

# index <- 1
# save(index, file=paste0("index", index, ".Rda"))


### shadowVIMP method - with pre-selection ----
proposed_with_preselect_deg_50 <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting2_column(z)}))

#small
causal <- add_degenhardt_informative_status(group_size = 50)
proposed_with_preselect_deg_50_small <- try(publication_results_proposed_with_preselect(proposed_with_preselect_deg_50,
                                                                                        alpha=0.05,
                                                                                        true_informatives = causal,
                                                                                        total_no_vars = 5000))

#savings
save_RDS_and_delete_object(proposed_with_preselect_deg_50_small, subfolder = "./results_supplementary/intermediate_results/deg50")
save_RDS_and_delete_object(proposed_with_preselect_deg_50, subfolder = "./results_supplementary/intermediate_results/deg50")
rm(causal)

# index <- index + 1
# save(index, file=paste0("index", index, ".Rda"))

## Nicodemus et al. (2010) ----
### shadowVIMP without pre-selection ----
proposed_without_preselect_nicodemus <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting23_column(z)}))

#small
causal <- add_nicodemeus_informative_status()
proposed_without_preselect_nicodemus_small <- try(publication_results_proposed_without_preselect(proposed_without_preselect_nicodemus,
                                                                                                 alpha=0.05,
                                                                                                 true_informatives=causal,
                                                                                                 total_no_vars = 12))

#savings
save_RDS_and_delete_object(proposed_without_preselect_nicodemus_small, subfolder = "./results_supplementary/intermediate_results/nicodemus")
save_RDS_and_delete_object(proposed_without_preselect_nicodemus, subfolder = "./results_supplementary/intermediate_results/nicodemus")
rm(causal)

# index <- index + 1
# save(index, file=paste0("index", index, ".Rda"))

### shadowVIMP with pre-selection ----
proposed_with_preselect_nicodemus <- parLapply(cl, 1:num.replicates, function(z) try({evaluatesetting24_column(z)}))

#small
causal <- add_nicodemeus_informative_status()
proposed_with_preselect_nicodemus_small <- try(publication_results_proposed_with_preselect(proposed_with_preselect_nicodemus,
                                                                                           alpha=0.05,
                                                                                           true_informatives=causal,
                                                                                           total_no_vars = 12))
#savings
save_RDS_and_delete_object(proposed_with_preselect_nicodemus_small, subfolder = "./results_supplementary/intermediate_results/nicodemus")
save_RDS_and_delete_object(proposed_with_preselect_nicodemus, subfolder = "./results_supplementary/intermediate_results/nicodemus")
rm(causal)

# index <- index + 1
# save(index, file=paste0("index", index, ".Rda"))

stopCluster(cl)
