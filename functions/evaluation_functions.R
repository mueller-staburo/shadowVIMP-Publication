#' Compute sensitivity, Type-1 error, FDR and FWER for a single replicate, both 
#' before and after p-value adjustment using Holm’s and the Benjamini–Hochberg 
#' procedure.
#'
#' @param one_replicate Named numeric vector with p-values. The names correspond to the analysed variables.
#' @param true_informatives Character vector containing the names of the
#'   informative covariates
#' @param total_no_vars Numeric, number of variables tested.
#' @param alpha Numeric, the significance level, must be between 0 and 1.
#' @param identifier 
#'
#' @returns
analyse_replicate <- function(one_replicate, true_informatives, total_no_vars, alpha, identifier) {
  
  selected_unadjusted <- names(one_replicate[one_replicate <= alpha])
  selected_fdr <- names(which(stats::p.adjust(one_replicate, method = "BH", n = total_no_vars) <= alpha))
  selected_fwer <- names(which(stats::p.adjust(one_replicate, method = "holm", n= total_no_vars) <= alpha))

  sensitivity_unadjusted <- get_sensitivity(selected_unadjusted, true_informatives)
  sensitivity_fdr <- get_sensitivity(selected_fdr, true_informatives)
  sensitivity_fwer <- get_sensitivity(selected_fwer, true_informatives)

  type1_error <- get_type1_error(selected = selected_unadjusted, true_informatives = true_informatives, total_no_vars)
  fdr <- get_FDR(selected = selected_fdr, true_informatives = true_informatives)
  fwer <- get_FWER(selected = selected_fwer, true_informatives = true_informatives)
  
  selections <- list("selected_unadjusted" = selected_unadjusted,
                     "selected_fdr" = selected_fdr,
                     "selected_fwer" = selected_fwer)
  
  performance <- data.frame(
    "identifier" = identifier,
    "sensitivity_unadjusted" = sensitivity_unadjusted,
    "sensitivity_fdr" = sensitivity_fdr,
    "sensitivity_fwer" = sensitivity_fwer,
    "type1_error" = type1_error,
    "fdr" = fdr,
    "fwer" = fwer,
    row.names = NULL)
  
  return(list("selections" = selections,
              "performance" = performance))
}

# Compute the p-values resulting from the proposed method
# Return a named numeric vector containing the p-values
from_proposed_result_to_small_result <- function(result_table_proposed) {
  p_vals <- 1-result_table_proposed[,2]
  varnames <- result_table_proposed$varname
  return(setNames(p_vals, varnames))
}

# Convert the 'Rejected' or 'Confirmed' decision resulting from the Boruta 
# method into 1 or 0 p-values
# Selected variables get p-value of 0, while rejected ones get p-value of 1
# Return a named numeric vector containing the p-values
from_boruta_to_small_result <- function(boruta_result) {
  varnames <- names(boruta_result$finalDecision)
  result_boruta <- as.character(boruta_result$finalDecision)
  result_boruta2 <- replace(result_boruta, result_boruta == "Rejected", 1)
  result_boruta3 <- replace(result_boruta2, result_boruta2 == "Confirmed", 0)
  return(setNames(result_boruta3, varnames))
}

# Convert the character decisions regarding each covariate resulting from the 
# rfvimptest into 0–1 p-values
# Selected variables get p-value of 0, while rejected ones get p-value of 1
# Return a named numeric vector containing the p-values
from_rfvimptest_to_small_result <- function(result_table_rfvimptest) {
  out <- sapply(result_table_rfvimptest, function(x) ifelse(x == "accept H1", 0, 1))
  return(out)
}

# Goal: extract the exact p-values obtained using the rfvimptest method
from_rfvimptest_to_small_result_pvalues <- function(result_table_rfvimptest) {
  out <- result_table_rfvimptest
  return(out)
}

# Extract the p-values obtained from the Janitza et al. (2018) method
# Return a named numeric vector containing the p-values
from_vita_to_small_result <- function(result_vita) {
  p_vals <- result_vita$pvalue
  varnames <- rownames(result_vita)
  return(setNames(p_vals, varnames))
}

#' Compute sensitivity
#'
#' @param selected Character vector containing the names of the covariates
#'   selected by the method
#' @param true_informatives Character vector containing the names of the
#'   informative covariates
#'
#' @returns Numeric value of sensitivity.
get_sensitivity <- function(selected, true_informatives) {
  N_TP <- length(intersect(true_informatives, selected))
  N_true_hypotheses <- length(true_informatives)
  if(N_true_hypotheses == 0 | N_TP == 0) {
    return(c("sensitivity" = 0)) 
  } 
  return(c("sensitivity" = N_TP/N_true_hypotheses))
}

#' Compute Type-1 error
#'
#' @param selected Character vector containing the names of the covariates
#'   selected by the method
#' @param true_informatives Character vector containing the names of the
#'   informative covariates 
#' @param total_no_vars Numeric, number of variables tested.
#'
#' @returns Numeric value of Type-1 error
get_type1_error <- function(selected, true_informatives, total_no_vars) {
  N_selected <- length(selected)
  N_TP <- length(intersect(true_informatives, selected))
  N_FP <- N_selected - N_TP
  N_false_hypotheses <- total_no_vars-length(true_informatives)
  
  if(N_false_hypotheses == 0) {
    return(c("type1_error" = "0"))
  }
  
  return(c("type1_error" = N_FP/N_false_hypotheses)) 
}

# Compute false discovery rate (FDR)
# Same argument as for `get_sensitivity()`
get_FDR <- function(selected, true_informatives) {
  N_selected <- length(selected)
  N_TP <- length(intersect(true_informatives, selected))
  N_FP <- N_selected - N_TP
  
  if(N_selected > 0) {
    return(c("FDR" = N_FP/N_selected)) 
  } else {
    return(c("FDR" = 0))
  }
}

# Compute family-wise error rate (FWER)
# Same argument as for `get_sensitivity()`
get_FWER <- function(selected, true_informatives) {
  N_selected <- length(selected)
  N_TP <- length(intersect(true_informatives, selected))
  N_FP <- N_selected - N_TP
  
  return(c("FWER" = ifelse(N_FP > 0, 1, 0))) 
}

# The following functions are designed to calculate  performance metrics 
# (sensitivity, Type-1 error, FDR and FWER) across all replicates produced by a given method.

# Apply the`analyse_replicate()` function to the results obtained from the Boruta method
publication_results_boruta <- function(raw_replicates_object, alpha, true_informatives, total_no_vars, identifier = deparse(substitute(raw_replicates_object))) { 
  result_list <- list()
  for(i in 1:length(raw_replicates_object)){
    result_list[[i]] <- analyse_replicate(from_boruta_to_small_result(raw_replicates_object[[i]]),
                                          true_informatives = true_informatives,
                                          total_no_vars = total_no_vars,
                                          alpha = alpha,
                                          identifier = identifier)
  }
  return(result_list)
}

# Apply the`analyse_replicate()` function to the results obtained from the Janitza et al. (2018) method
publication_results_vita <- function(raw_replicates_object, alpha, true_informatives, total_no_vars, identifier = deparse(substitute(raw_replicates_object))) { 
  result_list <- list()
  for(i in 1:length(raw_replicates_object)){
    result_list[[i]] <- analyse_replicate(from_vita_to_small_result(raw_replicates_object[[i]]),
                                          true_informatives = true_informatives,
                                          total_no_vars = total_no_vars,
                                          alpha = 0.05,
                                          identifier = identifier)
  }
  return(result_list)
}

# Apply the`analyse_replicate()` function to the results obtained from the `rfvimptest` method
# The results considered here are the 0–1 p-values
publication_results_rfvimptest <- function(raw_replicates_object, alpha, true_informatives, total_no_vars, identifier = deparse(substitute(raw_replicates_object))) { 
  result_list<- list()
  for(i in 1:length(raw_replicates_object)){
    result_list[[i]] <- analyse_replicate(from_rfvimptest_to_small_result(raw_replicates_object[[i]][["testres"]]),
                                          true_informatives = true_informatives,
                                          total_no_vars = total_no_vars,
                                          alpha = alpha,
                                          identifier = identifier)
  }
  return(result_list)
}

# Apply the`analyse_replicate()` function to the results obtained from the `rfvimptest` method
# The results considered here are the exact p-values
publication_results_rfvimptest_pvalues <- function(raw_replicates_object, alpha, true_informatives, total_no_vars, identifier = deparse(substitute(raw_replicates_object))) { 
  result_list<- list()
  for(i in 1:length(raw_replicates_object)){
    result_list[[i]] <- analyse_replicate(from_rfvimptest_to_small_result_pvalues(raw_replicates_object[[i]][["pvalues"]]),
                                          true_informatives = true_informatives,
                                          total_no_vars = total_no_vars,
                                          alpha = alpha,
                                          identifier = identifier)
  }
  return(result_list)
}

# Apply the`analyse_replicate()` function to the results obtained from the 
# proposed method that does NOT use the pre-selection of covariates
publication_results_proposed_without_preselect <- function(raw_replicates_object, alpha, true_informatives, total_no_vars, identifier = deparse(substitute(raw_replicates_object))) {
  result_list <- list()
  for(i in 1:length(raw_replicates_object)) { 
    for(k in c("per_variable", "pooled")) {
      for(j in c("without_correction", "with_correction")) {
        for(h in c("0_2", "0_4", "0_6", "0_8", "1"))  {
          
          if(h == "1") {
            out <- analyse_replicate(
              one_replicate = from_proposed_result_to_small_result(raw_replicates_object[[i]][[1]][["vimpermsim"]][["test_results"]][[j]][[k]][["0_05"]]),
              true_informatives = true_informatives,
              total_no_vars = total_no_vars,
              alpha = alpha,
              identifier = paste0("iterprop", h, "_", k, "_", j, "_", identifier))
          }
          
          if(h != "1") {
            out <- analyse_replicate(from_proposed_result_to_small_result(raw_replicates_object[[i]][[1]][["vimpermsim"]][["test_results"]][["iterations_prop"]][[h]][[j]][[k]][["0_05"]]),
                                     true_informatives = true_informatives,
                                     total_no_vars = total_no_vars,
                                     alpha = alpha,
                                     identifier = paste0("iterprop", h, "_", k, "_", j, "_", identifier))
          }
          result_list[[h]][[k]][[j]][[i]] <- out
          
        }}}}
  return(result_list)
}

# Apply the`analyse_replicate()` function to the results obtained from the 
# proposed method that does use the pre-selection of covariates
publication_results_proposed_with_preselect <- function(raw_replicates_object, alpha, true_informatives, total_no_vars, identifier = deparse(substitute(raw_replicates_object))) {
  result_list <- list()
  for(i in 1:length(raw_replicates_object)) { 
    for(k in c("per_variable", "pooled")) {
      for(j in c("without_correction", "with_correction")) {
        for(h in c("0_2", "0_4", "0_6", "0_8", "1"))  {
          
          if(h == "1") {
            out <- analyse_replicate(
              one_replicate = from_proposed_result_to_small_result(raw_replicates_object[[i]][[3]][["vimpermsim"]][["test_results"]][[j]][[k]][["0_05"]]),
              true_informatives = true_informatives,
              total_no_vars = total_no_vars,
              alpha = alpha,
              identifier = paste0("iterprop", h, "_", k, "_", j, "_", identifier))
          }
          
          if(h != "1") {
            out <- analyse_replicate(from_proposed_result_to_small_result(raw_replicates_object[[i]][[3]][["vimpermsim"]][["test_results"]][["iterations_prop"]][[h]][[j]][[k]][["0_05"]]),
                                     true_informatives = true_informatives,
                                     total_no_vars = total_no_vars,
                                     alpha = alpha,
                                     identifier = paste0("iterprop", h, "_", k, "_", j, "_", identifier))
          }
          result_list[[h]][[k]][[j]][[i]] <- out
          
        }}}}
  return(result_list)
}
