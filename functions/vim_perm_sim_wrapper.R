vim_perm_sim_wrapper <- function(alphas = c(0.3, 0.10, 0.05), nsims = c(30, 120, 1500), 
                                 entire_data,
                                 y, 
                                 permute = "rows",
                                 save_vim_history = "last",
                                 replace = T,
                                 scale.permutation.importance = T,
                                 write.forest = F,
                                 additional_alphas = NULL,  #only for benchmarking
                                 additional_iter_prop = NULL,
                                 num.threads = NULL,
                                 ...
                                 ) {
  
  #some checks
  if(!length(alphas) == length(nsims)){
    stop("alphas and nsims must have the same length!")
  }
  if(is.unsorted(rev(alphas)) | any(alphas <= 0 | any(alphas >= 1))) {
    stop("non-sensical alphas")
  }
  

  
  #number of variables, needed for BH and Holm adjustment
  M <- ncol(entire_data)-1
  
  #runtime start
  start_time <- Sys.time()
  
  
  #for loop over alphas
  replicate <- NULL
  for(j in 1:length(alphas)) {
    
    cat("alpha ", alphas[j], " \n")
    
    if(j>1 && length(replicate[[j-1]]$variables_remaining_for_replicate_pooled)==0) {
      cat("here")
      vimpermsim <- replicate[[j-1]]$vimpermsim
      result_from_previous_step_bool = TRUE
    } else{

    
    #run algorithm
    
    vimpermsim <-  vim_perm_sim(entire_data = entire_data %>% { if(j>1) 
      select(., any_of(c(replicate[[j-1]]$variables_remaining_for_replicate_pooled, "y"))) else .},  #pooled pre selection
      y=y, 
      permute = permute,
      nsim = nsims[j],
      model = "ranger",
      scale.permutation.importance = scale.permutation.importance,
      importance = "permutation",
      write.forest = write.forest,
      num.threads = num.threads,
      replace = replace,
      ...)
    result_from_previous_step_bool = FALSE
    }
    
    if(alphas[j] == min(alphas) && !is.null(additional_alphas)){
    for (k in additional_alphas) {
      vimpermsim <- add_test_results(vimpermsim, 
                                     M, 
                                     alpha = k, 
                                     remove_vim_history = F)
      
      vimpermsim <- add_test_results(vimpermsim, 
                                     M, 
                                     alpha = k,
                                     plus_one_plus_one_correction = T,
                                     remove_vim_history = F)
    }}
    
    if(alphas[j] == min(alphas) && !is.null(additional_iter_prop)){
      for (p in additional_iter_prop) {
        vimpermsim <- add_test_results(vimpermsim, 
                                       M, 
                                       alpha = alphas[j], 
                                       remove_vim_history = F,
                                       iterations_prop = p)
        
        vimpermsim <- add_test_results(vimpermsim, 
                                       M, 
                                       alpha = alphas[j], 
                                       plus_one_plus_one_correction = T,
                                       remove_vim_history = F,
                                       iterations_prop = p)
      }
      
    }
    
    vimpermsim <- add_test_results(vimpermsim, 
                                   M, 
                                   alpha = alphas[j], 
                                   remove_vim_history = F)
    
    vim_history_deletable = T
    if(length(vimpermsim$test_results$without_correction$pooled[[1]] %>% 
        filter(quantile_pooled >= 1-alphas[j]) %>% 
        select(varname) %>% 
        unlist() %>% 
        unname()) == 0) {vim_history_deletable = F}
    
    vimpermsim <- add_test_results(vimpermsim, 
                                   M, 
                                   alpha = alphas[j],
                                   plus_one_plus_one_correction = T,
                                   remove_vim_history = ifelse(!vim_history_deletable, F, ifelse(save_vim_history == "last" && alphas[j] == min(alphas), F,
                                                               ifelse(save_vim_history == "last" && alphas[j] != min(alphas), T,
                                                                      ifelse(save_vim_history == "everything", F, 
                                                                             ifelse(save_vim_history == "none", T, 
                                                                                    stop("invalid parameter for save_vim_history. Use either 'last', 'everything' or 'none'")))))))
    

    
    variables_remaining_for_replicate <- vimpermsim$test_results$without_correction$per_variable[[1]] %>% 
      filter(quantile_per_variable >= 1-alphas[j]) %>% 
      select(varname) %>% 
      unlist() %>% 
      unname()
    variables_remaining_for_replicate_pooled <- vimpermsim$test_results$without_correction$pooled[[1]] %>% 
      filter(quantile_pooled >= 1-alphas[j]) %>% 
      select(varname) %>% 
      unlist() %>% 
      unname()
    cat("Variables remaining: ", length(variables_remaining_for_replicate_pooled), "\n")
    
    #runtime end
    end_time <- Sys.time()
    
    replicate[[j]] <- list(#"replicate_id" = i,
                           #"seed_list" = seed_list[[i]],
                           "alpha" = alphas[j],
                           "result_taken_from_previous_step" = result_from_previous_step_bool,
                           "vimpermsim" = vimpermsim,
                           "variables_remaining_for_replicate" = variables_remaining_for_replicate,
                           "variables_remaining_for_replicate_pooled" = variables_remaining_for_replicate_pooled,
                           "time_elapsed" = difftime(end_time, start_time, units = "mins"))
  
  }
  return(replicate)
}
