add_causal_status_to_result <- function(result, causal_vars){
  return(result %>% mutate(causal_indicator = factor(ifelse(varname %in% causal_vars, 1, 0), levels = c("0", "1"))))
}
