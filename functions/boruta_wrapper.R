#' A wrapper for the [Boruta::Boruta] function which restructures the outcome.
#'
#' @param data Data frame of predictors
#' @param formula Formula describing model to be analysed
#' @param consistency_parallel Boolean, if `TRUE`the response column is attached
#'   to the results.
#' @param remove_vim_history Boolean, if `TRUE` the full importance history is
#'   removed from the outcome object
#' @param num.trees Numeric, number of trees.
#' @param num.threads Numeric, the number of threads used for parallel tree
#'   building
#' @param ... Additional parameters passed to the [Boruta::Boruta].
#'
#' @seealso \code{\link[Boruta]{Boruta}},
#' \code{\link[Boruta]{TentativeRoughFix}}
#'
#' @returns Data frame containing the results of the [Boruta::Boruta] function.
boruta_wrapper <- function(data, formula = y~., 
                           consistency_parallel = T, 
                           remove_vim_history = F, 
                           num.trees = max(2*(ncol(data)-1), 10000),
                           num.threads = 5,
                           ...) {
res <- Boruta::TentativeRoughFix(Boruta::Boruta(formula, data, num.trees = num.trees, num.threads = num.threads, ...))
res$decis <- data.frame(variable = names(res$finalDecision), decision_boruta = res$finalDecision)
if(consistency_parallel) {res$consistency_parallel <- data[,1]}
if(remove_vim_history) {res$ImpHistory <- NULL}
return(res)
}

