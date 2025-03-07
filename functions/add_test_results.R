add_test_results <- function(vimpermsim, M, alpha = 0.05, 
                             plus_one_plus_one_correction = F,
                             remove_vim_history = F, 
                             iterations_prop = 1) {
  # M<-5000
  # alpha =0.05
  # fdr=alpha
  # fwer=alpha
  
  if(iterations_prop < 1) {
    iters_to_use <- ceiling(iterations_prop * nrow(vimpermsim$vim_simulated))
    vim_simulated <- vimpermsim$vim_simulated[1:iters_to_use, ]
    cat("Using ", iterations_prop, " of all iterations", "! \n")
  } else {
    vim_simulated <- vimpermsim$vim_simulated
  }
  
  #split data
  originals <- vim_simulated %>% 
    select(-ends_with("_permuted"))
  shadows <- vim_simulated %>% 
    select(ends_with("_permuted"))
  
  
  #define 1-alpha threshholds 
  cutoffs <- data.frame("Type1" = sapply(seq(1:M), function(x) 1 - (alpha))[1:ncol(originals)],
                        "FDR"   = sapply(seq(1:M), function(x) 1 - (alpha * x/M))[1:ncol(originals)], #BH
                        "FWER"  = sapply(seq(1:M), function(x) 1 - (alpha * 1/(M+1-x)))[1:ncol(originals)]) #HOLM

  #calculate medians of original variables
  medians <- originals %>% summarise_all(median)
  varnames_originals <- names(medians)
  
  #compared medians to quantiles of respective shadow variable
  quants_per_variable <- lapply(varnames_originals, function(x) {
    ec<- {
      if(plus_one_plus_one_correction) { 
      ecdf(c(shadows[[paste0(x, "_permuted")]], Inf))
      } 
      else { ecdf(shadows[[paste0(x, "_permuted")]]) 
        }
    }
    ec(medians[[x]]) 
  }
  )
  
  quants_per_variable_df <- data.frame("varname" = varnames_originals, 
                                       "quantile_per_variable" = unlist(quants_per_variable)) %>% 
    arrange(desc(quantile_per_variable)) #sort 1-alpha values for rank based FDR,FWER control
  quants_per_variable_df <- quants_per_variable_df <- cbind(quants_per_variable_df, cutoffs) %>% 
    mutate(Type1_confirmed = ifelse(Type1 <= quantile_per_variable, 1, 0))
  
  
  #find first occurence of adjusted-pvalue < observed pvalue
  first_negative_FDR <- which(quants_per_variable_df$quantile_per_variable - quants_per_variable_df$FDR < 0)[1]
  first_negative_FWER <- which(quants_per_variable_df$quantile_per_variable - quants_per_variable_df$FWER < 0)[1]
  
  
  #assign all variables before the first occurence 1, else 0, if there is no occurence, then all variables are confirmed
  if(!is.na(first_negative_FDR)) {
    quants_per_variable_df$FDR_confirmed <- c(rep(1, times = first_negative_FDR-1), rep(0, nrow(quants_per_variable_df)-first_negative_FDR+1))} else{
      quants_per_variable_df$FDR_confirmed <- 1
    }
  
  if(!is.na(first_negative_FWER)) {
    quants_per_variable_df$FWER_confirmed <- c(rep(1, times = first_negative_FWER-1), rep(0, nrow(quants_per_variable_df)-first_negative_FWER+1))} else{
      quants_per_variable_df$FWER_confirmed <- 1
    }
  
  
  #compare medians to pooled quantiles 
  quants_pooled_df <- NULL

    quants_pooled_fun <- function(varnames_originals) {
      
      #compute standard deviations to make shadows of variables with different cardinalities comparable
      sd_of_shadows <- sapply(shadows, sd)
      mean_of_shadows <- sapply(shadows, mean)
      
      #divide medians and shadows by sd of shadow

          medians_divided_sd <- (medians-mean_of_shadows)/sd_of_shadows
          shadows_centered <- sweep(shadows, 2, mean_of_shadows, FUN = "-")
          shadows_divided_sd <- sweep(shadows_centered, 2, sd_of_shadows, FUN = "/")

      
      #ecdf function
      ec_pool <- {if(plus_one_plus_one_correction) {
        ecdf(c(unlist(shadows_divided_sd), Inf))} else {
        ecdf(c(unlist(shadows_divided_sd)))
        }}
      
      #return for all varnames remaining
      return(lapply(varnames_originals, function(x) {
        ec_pool(medians_divided_sd[[x]])
      }))
    }
    quants_pooled <- quants_pooled_fun(varnames_originals)
    quants_pooled_df <- data.frame("varname" = varnames_originals, 
                                   "quantile_pooled" = unlist(quants_pooled)) %>% 
      arrange(desc(quantile_pooled)) #sort 1-alpha values for rank based FDR,FWER control
    quants_pooled_df <- quants_pooled_df <- cbind(quants_pooled_df, cutoffs)%>% 
      mutate(Type1_confirmed = ifelse(Type1 <= quantile_pooled, 1, 0))
    
    #find first occurence of adjusted-pvalue < observed pvalue
    first_negative_FDR <- which(quants_pooled_df$quantile_pooled - quants_pooled_df$FDR < 0)[1]
    first_negative_FWER <- which(quants_pooled_df$quantile_pooled - quants_pooled_df$FWER < 0)[1]
    
    #assign all variables before the first occurence 1, else 0, if there is no occurence, then all variables are confirmed
    if(!is.na(first_negative_FDR)) {
      quants_pooled_df$FDR_confirmed <- c(rep(1, times = first_negative_FDR-1), rep(0, nrow(quants_pooled_df)-first_negative_FDR+1))} else{
        quants_pooled_df$FDR_confirmed <- 1
      }
    
    if(!is.na(first_negative_FWER)) {
      quants_pooled_df$FWER_confirmed <- c(rep(1, times = first_negative_FWER-1), rep(0, nrow(quants_pooled_df)-first_negative_FWER+1))} else{
        quants_pooled_df$FWER_confirmed <- 1
      }

    
    
  
  
  if(!iterations_prop == 1){
    
    if(plus_one_plus_one_correction){
    vimpermsim$test_results$iterations_prop[[gsub("\\.", "_",as.character(iterations_prop))]]$with_correction$per_variable[[gsub("\\.", "_",as.character(alpha))]] <- quants_per_variable_df
    vimpermsim$test_results$iterations_prop[[gsub("\\.", "_",as.character(iterations_prop))]]$with_correction$pooled[[gsub("\\.", "_",as.character(alpha))]] <- quants_pooled_df}
    else{
    vimpermsim$test_results$iterations_prop[[gsub("\\.", "_",as.character(iterations_prop))]]$without_correction$per_variable[[gsub("\\.", "_",as.character(alpha))]] <- quants_per_variable_df
    vimpermsim$test_results$iterations_prop[[gsub("\\.", "_",as.character(iterations_prop))]]$without_correction$pooled[[gsub("\\.", "_",as.character(alpha))]] <- quants_pooled_df}
    
  } else {
    if(plus_one_plus_one_correction){
      vimpermsim$test_results$with_correction$per_variable[[gsub("\\.", "_",as.character(alpha))]] <- quants_per_variable_df
      vimpermsim$test_results$with_correction$pooled[[gsub("\\.", "_",as.character(alpha))]] <- quants_pooled_df}
    else{
      vimpermsim$test_results$without_correction$per_variable[[gsub("\\.", "_",as.character(alpha))]] <- quants_per_variable_df
      vimpermsim$test_results$without_correction$pooled[[gsub("\\.", "_",as.character(alpha))]] <- quants_pooled_df}
  }
    
  if(remove_vim_history) {vimpermsim$vim_simulated <- NULL}
  return(vimpermsim)
  
}