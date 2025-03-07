summarise_result <- function(result, replicate_no = NA, seed_list = NA, pooled = NA) {
  
  result %>% 
    select(varname, Type1_confirmed, FDR_confirmed, FWER_confirmed, causal_indicator) %>% 
    tidyr::pivot_longer(cols = c(Type1_confirmed, FDR_confirmed, FWER_confirmed), 
                        names_to = "adjustment_method",
                        values_to = "confirmation") %>% #long format
    group_by(adjustment_method, causal_indicator, .drop = FALSE) %>% 
    filter(confirmation == 1) %>% 
    summarise(n_confirmed = n()) %>% 
    mutate(total_vars_confirmed = sum(n_confirmed)) %>%
    mutate(proportion = tidyr::replace_na(n_confirmed/total_vars_confirmed, 0)) %>% 
    mutate(replicate_no = replicate_no,
           seed_list = seed_list,
           pooled = pooled,
           .before = everything()[1])
}

