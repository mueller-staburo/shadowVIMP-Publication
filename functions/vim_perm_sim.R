#' Compute the variable importance of the predictors and their row-wise shadows
#'
#' `vim_perm_sim()` calculates repeatedly (`niters` times) the variable
#' importance of the original values of the predictors and their row-wise
#' permuted shadows. Each shadow's variable importance is computed based on a
#' new permutation of the initial predictor values.
#'
#' @param entire_data Input data frame, outcome variable should be stored in the
#'   column called "y".
#' @param y Name of the column with the outcome.
#' @param nsim Numeric, number of permutations of the initial predictor values,
#'   default is 100.
#' @param permute Character, one of: `"rows"` or `"columns"`, which specifies
#'   whether the data should be permuted in rows or columns. Default is
#'   `"rows"`.
#' @param importance Character, the type of variable importance to be calculated
#'   for each independent variable. Argument passed to [ranger::ranger()],
#'   default is `permutation`.
#' @param replace Boolean passed to [ranger::ranger()], specifies whether to
#'   sample with or without replacement.
#' @param model Character, one of `"cforest"` or `"cforest"`, specifies which
#'   random forest implementation should be used.
#' @param scale.permutation.importance Boolean passed to [ranger::ranger()],
#'   scale permutation importance by standard error.
#' @param num.threads Numeric. The number of threads used by [ranger::ranger()]
#'   for parallel tree building.
#' @param write.forest Boolean passed to [ranger::ranger()], if `TRUE`, the
#'   fitted object is saved (required for making predictions).
#' @param num.trees Numeric, number of trees. Passed to [ranger::ranger()],
#'   default is `max(2 * (ncol(data) - 1), 10000)`.
#' @param df_name Character, name of the object passed as `entire_data`.
#' @param ... Additional parameters.
#'
#' @returns List containing `nsim` variable importance values for both the
#'   original and row-wise permuted predictors along with the control variables
#'   used in the function call.
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

  p <- ncol(entire_data)-1
  n <- nrow(entire_data)

  #Splitting predictors
  predictors <- entire_data %>% select(- c(y))
  
  if(length(grep('$_permuted',names(predictors)))>0){
    stop('One or more variables ending with _permuted. Please rename them.')
  }
  
  #Creating permuted predictors
  predictors_p <- if(permute == "rows") {
    predictors[sample(1:n),,drop=FALSE]
  } else if(permute == "columns") {
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
  df_sim <- as.data.frame(do.call(rbind, vimp_sim))

  # Storing result to be returned
  res <- list(vim_simulated = df_sim,
              controls = list(..., nsim = nsim, permute = permute), 
              consistency_parallel = entire_data[,1])
}