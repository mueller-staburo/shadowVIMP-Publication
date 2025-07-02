#' Compute formatted as percentages mean of a vector along with the bootstrap 
#' confidence interval
#'
#' @param x Numeric vector
#'
#' @returns Character summarizing the mean and a bootstrap confidence interval 
#' of `x`, all formatted as percentages. 
tfl_output <- function(x) {
  mean_val <- sprintf("%.1f%%",mean(x, na.rm = TRUE)*100)  
  boot <- bootstrapf(x)
  lower <- sprintf("%.1f%%",boot[[1]][["lower"]] * 100)
  upper <- sprintf("%.1f%%",boot[[1]][["upper"]] * 100)
  result <- paste0(mean_val, " [", lower, ";", upper, "]") 
  return(result)
}

#' Use the non-parametric bootstrap method with 100,000 resamples to obtain 
#' confidence limits for the population mean without assuming normality
#'
#' @param vector_to_boot Numeric vector
#'
#' @returns A named list containing the estimated mean, along with lower and 
#' upper confidence limits. 
bootstrapf <- function(vector_to_boot) {
  ret<- list(setNames(unlist(Hmisc::smean.cl.boot(vector_to_boot, B=100000)), c("mean", "lower", "upper")))
  return(ret)
}
