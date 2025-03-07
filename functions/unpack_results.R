#not proposed:
performance_others <- function(dat_result) {
  perf <- data.frame(data.table::rbindlist(lapply(dat_result, `[[`, 2))) %>% mutate(type = NA,
                                                                                    iterprop = NA,
                                                                                    correction = NA, .before = everything())  
  return(perf)
}

#performance propose:
performance_proposed <- function(dat_result, iterprop = "1", type = "pooled", correction = "with_correction") {
  perf <- data.frame(data.table::rbindlist(lapply(dat_result[[iterprop]][[type]][[correction]], `[[`, 2))) %>% mutate(type = type,
                                                                                                                      iterprop = iterprop,
                                                                                                                      correction = correction, .before = everything())  
  return(perf)
}

#select other
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

#select proposed
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
