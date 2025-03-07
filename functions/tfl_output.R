tfl_output <- function(x) {
  mean_val <- sprintf("%.1f%%",mean(x, na.rm = TRUE)*100)  
  boot <- bootstrapf(x)
  lower <- sprintf("%.1f%%",boot[[1]][["lower"]] * 100)
  upper <- sprintf("%.1f%%",boot[[1]][["upper"]] * 100)
  result <- paste0(mean_val, " [", lower, ";", upper, "]") 
  return(result)
}


