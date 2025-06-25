#' Select influential covariates in random forests using multiple testing
#' control
#' 
#' #' The `vim_perm_sim_wrapper()` function by default performs variable selection in
#' multiple steps. Initially, it prunes the set of predictors using a relaxed
#' (higher) alpha threshold in a pre-selection stage. Variables that pass this
#' stage then undergo a final evaluation using the target (lower) alpha
#' threshold and more iterations. This stepwise approach distinguishes
#' informative from uninformative covariates based on their VIMPs and enhances
#' computational efficiency. The user can also perform variable selection in a
#' single step, without a pre-selection phase.
#'
#' @param alphas Numeric vector, significance level values for each step of the
#'   procedure, default `c(0.3, 0.10, 0.05)`.
#' @param nsims  Numeric vector, number of permutations to be performed in each
#'   step of the procedure, default `c(30, 120, 1500)`.
#' @param entire_data Input data frame.
#' @param y Name of the column with the outcome.
#' @param permute Character, one of: `"rows"` or `"columns"`, which specifies
#'   whether the data should be permuted in rows or columns. Default is
#'   `"rows"`.
#' @param save_vim_history Character, specifies which variable importance
#'   measures to save. Possible values are:
#'  * `"all"` - save variable importance measures from all steps
#'   of the procedure (both the pre-selection phase and the final selection
#'   step).
#'  * `"last"`(the default) - save only the variable importance measures from the final
#'   step.
#'  * `"none"` - do not save any variable importance measures.
#' @param replace Boolean passed to [ranger::ranger()], specifies whether to
#'   sample with or without replacement.
#' @param scale.permutation.importance Boolean passed to [ranger::ranger()],
#'   scale permutation importance by standard error.
#' @param write.forest Boolean passed to [ranger::ranger()], if `TRUE`, the
#'   fitted object is saved (required for making predictions). 
#' @param additional_alphas Numeric
#' @param additional_iter_prop Numeric 
#' @param num.threads Numeric. The number of threads used by [ranger::ranger()]
#'   for parallel tree building.
#' @param ... Additional parameters.
#'
#' @returns List
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
  
  # Checks of input parameters
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
    
    replicate[[j]] <- list("alpha" = alphas[j],
                           "result_taken_from_previous_step" = result_from_previous_step_bool,
                           "vimpermsim" = vimpermsim,
                           "variables_remaining_for_replicate" = variables_remaining_for_replicate,
                           "variables_remaining_for_replicate_pooled" = variables_remaining_for_replicate_pooled,
                           "time_elapsed" = difftime(end_time, start_time, units = "mins"))
  
  }
  return(replicate)
}
