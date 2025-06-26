#' Prepare the results from the non-proposed methods in order to calculate the
#' sensitivity, Type-1 error, FDR and FWER
#'
#' @param dat_result List with the results obtained using non-proposed methods
#' @returns Data frame with the results and placeholder columns
performance_others <- function(dat_result) {
  perf <- data.frame(data.table::rbindlist(lapply(dat_result, `[[`, 2))) %>% mutate(type = NA,
                                                                                    iterprop = NA,
                                                                                    correction = NA, .before = everything())  
  return(perf)
}

#' Prepare the results from the proposed method in order to calculate the
#' sensitivity, Type-1 error, FDR and FWER
#'
#' @param dat_result List with the results obtained using the proposed method
performance_proposed <- function(dat_result, iterprop = "1", type = "pooled", correction = "with_correction") {
  perf <- data.frame(data.table::rbindlist(lapply(dat_result[[iterprop]][[type]][[correction]], `[[`, 2))) %>% mutate(type = type,
                                                                                                                      iterprop = iterprop,
                                                                                                                      correction = correction, .before = everything())  
  return(perf)
}

# Compute the selection frequencies of covariates using unadjusted p-values and 
# p-values adjusted by Holm and Benjamini–Hochberg procedures, p-values obtained
# by using non-proposed methods
selections_others <- function(dat_result) {
  label <- dat_result[[1]][["performance"]][["identifier"]]
  select_unadjusted <- data.frame((table(c("dummy",unlist(lapply(lapply(dat_result, `[[`, 1), `[[`, "selected_unadjusted")))))) %>% mutate(identifier = label,
                                                                                                                                adjustment = "unadjusted",
                                                                                                                                type = NA,
                                                                                                                                iterprop = NA,
                                                                                                                                correction = NA, 
                                                                                                                                .before=everything()) %>% tidyr::pivot_wider(names_from = "Var1", values_from = "Freq")
  
  select_fdr <- data.frame((table(c("dummy", unlist(lapply(lapply(dat_result, `[[`, 1), `[[`, "selected_fdr")))))) %>% mutate(identifier = label,
                                                                                                                  adjustment = "BH",
                                                                                                                  type = NA,
                                                                                                                  iterprop = NA,
                                                                                                                  correction = NA,
                                                                                                                  .before=everything())%>% tidyr::pivot_wider(names_from = "Var1", values_from = "Freq")
  
  
  select_fwer <- data.frame((table(c("dummy", unlist(lapply(lapply(dat_result, `[[`, 1), `[[`, "selected_fwer")))))) %>% mutate(identifier = label,
                                                                                                                    adjustment = "Holm",
                                                                                                                    type = NA,
                                                                                                                    iterprop = NA,
                                                                                                                    correction = NA,
                                                                                                                    .before=everything())%>% tidyr::pivot_wider(names_from = "Var1", values_from = "Freq")
  
  return(data.table::rbindlist(list(select_unadjusted, select_fdr, select_fwer), fill = T))
}

# Compute the selection frequencies of covariates using unadjusted p-values and 
# p-values adjusted by Holm and Benjamini–Hochberg procedures, p-values obtained
# by using proposed method (shadowVIMP)
selections_proposed <- function(dat_result, iterprop = "1", type = "pooled", correction = "with_correction") {
  label <- dat_result[[iterprop]][[type]][[correction]][[1]][["performance"]][["identifier"]]
  select_unadjusted <- data.frame((table(c("dummy", unlist(lapply(lapply(dat_result[[iterprop]][[type]][[correction]], `[[`, 1), `[[`, "selected_unadjusted")))))) %>% mutate(identifier = label,
                                                                                                                                                                  adjustment = "unadjusted",
                                                                                                                                                                  type = type,
                                                                                                                                                                  iterprop = iterprop,
                                                                                                                                                                  correction = correction, 
                                                                                                                                                                  .before=everything()) %>% tidyr::pivot_wider(names_from = "Var1", values_from = "Freq")
  
  select_fdr <- data.frame((table(c("dummy", unlist(lapply(lapply(dat_result[[iterprop]][[type]][[correction]], `[[`, 1), `[[`, "selected_fdr")))))) %>% mutate(identifier = label, 
                                                                                                                                                    adjustment = "BH",
                                                                                                                                                    type = type,
                                                                                                                                                    iterprop = iterprop,
                                                                                                                                                    correction = correction, .before=everything())%>% tidyr::pivot_wider(names_from = "Var1", values_from = "Freq")
  
  
  select_fwer <- data.frame((table(c("dummy",unlist(lapply(lapply(dat_result[[iterprop]][[type]][[correction]], `[[`, 1), `[[`, "selected_fwer")))))) %>% mutate(identifier = label, 
                                                                                                                                                      adjustment = "Holm",
                                                                                                                                                      type = type,
                                                                                                                                                      iterprop = iterprop,
                                                                                                                                                      correction = correction, .before=everything())%>% tidyr::pivot_wider(names_from = "Var1", values_from = "Freq")
  
  return(data.table::rbindlist(list(select_unadjusted, select_fdr, select_fwer), fill = T))
}
