selection_helper <- function(vector_of_results) {
  selected <- as.data.frame(matrix(rep(1, length(vector_of_results)), nrow=1))
  names(selected) <- vector_of_results
  return(selected)
}