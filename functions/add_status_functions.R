# File with 'add_status' functions 

#' List the names of the predictor variables simulated from the latent variables 
#' that are informative for the outcome (data simulation design
#'  Degenhardt et al.(2019))
#'
#' @param group_size Numeric, parameter specifying the group size used in the 
#' Degenhardt et al.(2019) data simulation design
#'
#' @returns A character vector containing the names of the correlated predictor
#'  variables simulated from the three latent variables that are truly 
#'  informative for the outcome.
add_degenhardt_informative_status <- function(group_size = 50) {
  vars <- names(simulation.data.cor(100, rep(group_size, 6),  5000))
  return(vars[stringr::str_which(vars, "^g.1|^g.2|^g.3")])
}


#' List the names of the informative variables in Friedman (1991) data 
#' simulation design
#'
#' @returns A character vector with the names of informative variables
add_friedmann_informative_status <- function() {
  return(c("x.1", "x.2", "x.3", "x.4", "x.5"))
}


#' List the names of informative predictors in Nicodemus et al. (2010) data 
#' simulation design
#'
#' @returns NULL meaning that none of the predictors are informative for the 
#' outcome since the null case from the study of Nicodemus et al. (2010) is used
add_nicodemeus_informative_status <- function() {
  return(c())
}

#' Return the name of the informative predictor in Strobl et al. (2007) data 
#' simulation design
#'
#' @param return_causal 
#'
#' @returns A character specifying the name of the informative predictor.
add_strobl_informative_status <- function() {
  return(c("X2"))
}
