vim_perm_sim <- function(entire_data, 
                         y, 
                         nsim = 100, 
                         permute = "rows",
                         importance = "permutation", 
                         replace = T, 
                         model = "ranger",
                         scale.permutation.importance = T,
                         num.threads = NULL,
                         write.forest = F,
                         num.trees = max(2*(ncol(entire_data)-1), 10000),
                         df_name = "X", 
                         ...) {

  #entire_data <- data[,1:2]
  p <- ncol(entire_data)-1
  n <- nrow(entire_data)
  # additional_args <- list(...)
  # if(is.null(additional_args$mtry)) {
  #   mtry <- floor(sqrt(2*p))
  # }
  # if(is.null(c(additional_args$num.trees, additional_args$ntree)[1])) {
  #   num.trees = max(p, 10000)
  # }
  # entire_data <- simulation.data.cor(100, rep(50,6), 5000)
  # permute = "columns"
  #Splitting predictors
  predictors <- entire_data %>% select(- c(y))
  
  if(length(grep('$_permuted',names(predictors)))>0){
    stop('One or more variables ending with _permuted. Please rename them.')
  }
  
  #Creating permuted predictors
  predictors_p <- if(permute == "rows") {
    predictors[sample(1:n),,drop=FALSE]
  } else if(permute == "columns") {
    #predictors %>% mutate(across(everything(), sample))
    data.frame(lapply(predictors, sample))
  } else {
      stop("invalid argument for permute")
    }
  names(predictors_p)<-paste0(names(predictors_p), "_permuted")
  
  #Concatenating predictors, permuted predictors and label
  dt <- cbind.data.frame(predictors, predictors_p) %>% cbind.data.frame(entire_data %>% select(y)) 
  
  # Simulation:
  vimp_sim <- NULL
  dif_sim <- NULL

  
  
  for (i in 1:nsim){
    if((i %% 50 == 0) | (i==1)) {
      cat(paste0(format(Sys.time()), ": dataframe=", df_name, " nsim=", nsim, " num.trees=", num.trees,". Running step ", i, "\n"))
    }
    
    # dat_RF_rand <- predictors_p[sample(1:n),] 
    # dat_RF_bind <- cbind.data.frame(entire_data, dat_RF_rand)  
    

    #reshuffle row wise
    dt[,(ncol(predictors_p)+1):(2*ncol(predictors_p))] <- if(permute == "rows") {
      dt[sample(1:n), (ncol(predictors_p)+1):(2*ncol(predictors_p))]
    } else if(permute == "columns") {
      data.frame(lapply(dt[,(ncol(predictors_p)+1):(2*ncol(predictors_p))], sample))
    } else {
      stop("invalid argument for permute")
    }
    
    if(model == "cforest") {
    cf <- cforest(data = dt, formula = y~., controls = cforest_unbiased(...))
    
    vimp_sim[[i]]<-varimp(cf, nperm = 1)
    
    }
    
    if(model == "ranger") {
      # range <- ranger::ranger(formula = y~., data= dt,
      #                         importance = importance,
      #                         replace = replace, ...)
      vimp_sim[[i]] <- (ranger::ranger(y = dt$y, x = dt %>% select(-y),
                                       importance = importance,
                                       replace = replace,
                                       num.trees = num.trees,
                                       scale.permutation.importance = scale.permutation.importance,
                                       num.threads = num.threads,
                                       write.forest = write.forest,
                                       ...))$variable.importance %>% t() %>% as.data.frame() 
    }
  }
  
  
  # putting all results in a df and creating columns for the tests and the difference
  df_sim <- as.data.frame(do.call(rbind, vimp_sim))# %>% mutate(iter = 1:nsim, .before = everything()[1])

  

  
  
  # Storing result to be returned
  res <- list(vim_simulated = df_sim,
              controls = list(..., nsim = nsim, permute = permute), 
              consistency_parallel = entire_data[,1])
}


