### funs
#Input one_replicate is named vector with p-values. The name must be the corresponding variable. Boruta and rfvimp (here, not in general) do not
#return p-values, but binary decisions about selecting each variable. As such, selected variables get p-value of 0, while rejected ones get p-value of 1.
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





from_proposed_result_to_small_result <- function(result_table_proposed) {
  p_vals <- 1-result_table_proposed[,2]
  varnames <- result_table_proposed$varname
  return(setNames(p_vals, varnames))
}


from_boruta_to_small_result <- function(boruta_result) {
  varnames <- names(boruta_result$finalDecision)
  result_boruta <- as.character(boruta_result$finalDecision)
  result_boruta2 <- replace(result_boruta, result_boruta == "Rejected", 1)
  result_boruta3 <- replace(result_boruta2, result_boruta2 == "Confirmed", 0)
  return(setNames(result_boruta3, varnames))
}

from_rfvimptest_to_small_result <- function(result_table_rfvimptest) {
  out <- sapply(result_table_rfvimptest, function(x) ifelse(x == "accept H1", 0, 1))
  return(out)
}


from_vita_to_small_result <- function(result_vita) {
  p_vals <- result_vita$pvalue
  varnames <- rownames(result_vita)
  return(setNames(p_vals, varnames))
}

get_sensitivity <- function(selected, true_informatives) {
  N_TP <- length(intersect(true_informatives, selected))
  N_true_hypotheses <- length(true_informatives)
  if(N_true_hypotheses == 0 | N_TP == 0) {
    return(c("sensitivity" = 0)) 
  } 
  return(c("sensitivity" = N_TP/N_true_hypotheses))
}


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

get_FWER <- function(selected, true_informatives) {
  N_selected <- length(selected)
  N_TP <- length(intersect(true_informatives, selected))
  N_FP <- N_selected - N_TP
  
  return(c("FWER" = ifelse(N_FP > 0, 1, 0))) 
}


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
