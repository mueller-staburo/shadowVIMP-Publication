# File with the code for different data simulation designs used in the paper

#' Simulate data according to Nicodemus et al. (2010) simulation design
#'
#' @returns Data frame with simulated data
nicodemeus_sim <- function() {
  
  Cov <- matrix(0, 12, 12)
  Cov[1:4, 1:4] <- 0.9
  diag(Cov)[] <- 1
  beta <- c(5, 5, 2, 0, -5, -5, -2, 0, 0, 0, 0, 0)
  nsites <- 100
  
  X <- MASS::mvrnorm(nsites, rep(0, 12), Sigma = Cov)
  Y <- X %*% beta + rnorm(nsites, 0, 0.5)
  df <- cbind(y = Y, as.data.frame(X))
  
  return(df)
}

#' Generates simulated gene expression data in a regression setting.
#'
#' The underlying simulation model is described in detail in the paper
#' Degenhardt et al. (2019) paper (see README for exact reference). In brief, a
#' nonlinear regression model based on three uniformly distributed variables is
#' used. Predictor variables are simulated to be correlated with one of those
#' functional variables. In addition, independent, uniformly distributed
#' predictor variables are simulated.
#'
#' @param no.samples number of samples.
#' @param group.size number of variables in each of the six groups of correlated
#'   variables.
#' @param no.var.total total number of variables.
#' @param null simulate null model (using independent functional variables).
#'
#' @return A data.frame with samples in rows and variables in columns (Note:
#'   first column contains simulated phenotype). Variables are named as y (=
#'   phenotype), g.i.j (= variable j in group i) and ind.k (= k-th independent
#'   variable).
#'
#' @examples
#' # simulate toy data set
#' data = simulation.data.cor(no.samples = 100, group.size = rep(10, 6), no.var.total = 200)
#'
#' @export

simulation.data.cor <- function(no.samples, group.size, no.var.total, null = FALSE) {
  
  if (length(group.size) != 6) {
    stop("group.size needs to be a vector of length 6!")
  }
  ## simulate functional variables
  x = sapply(1:6, function(i) {
    #    rnorm(n = no.samples, mean = 0, sd = 1)})
    runif(n = no.samples, min = 0, max = 1)})
  colnames(x) = paste("x", 1:6, sep = ".")
  
  ## simulate groups of correlated variables based on functional variables
  g = lapply(1:3, function(i) {
    v = sapply(1:group.size[i], function(j) {
      x[, i] + 0.01 + 0.5 * (j - 1) / (group.size[i] - 1)  *
        rnorm(n = no.samples, mean = 0, sd = 0.3)
    })
    colnames(v) = paste("g", i, 1:group.size[i], sep = ".")
    return(v)
  })
  g = do.call(cbind, g)
  
  ## simulate groups of correlated variables based on null variables
  g.0 = lapply(1:3, function(i) {
    v = sapply(1:group.size[i], function(j) {
      x[, i+3] + 0.01 + 0.5 * (j - 1) / (group.size[i] - 1)  *
        rnorm(n = no.samples, mean = 0, sd = 0.3)
      
    })
    colnames(v) = paste("g.0", i, 1:group.size[i], sep = ".")
    return(v)
  })
  g.0 = do.call(cbind, g.0)
  
  if (ncol(g) + ncol(g.0) >= no.var.total) {
    warning("No additional independent variables are simulated!")
    ind = NULL
  } else {
    ind = matrix(runif(no.samples * (no.var.total - (ncol(g) + ncol(g.0))),
                       min = 0, max = 1),
                 nrow = no.samples)
    colnames(ind) = paste("ind", 1:ncol(ind), sep = ".")
  }
  
  ## phenotype
  if (null) {
    x.use = sapply(1:6, function(i) {
      runif(n = no.samples, min = 0, max = 1)})
    
  } else {
    x.use = x[, 1:3]  
  }
  
  y = 0.25 * exp(4 * x.use[, 1]) + 4 / (1 + exp(-20 * (x.use[, 2] - 0.5))) + 3 * x.use[, 3] +
    rnorm(n = no.samples, mean = 0, sd = 0.2)
  
  ## data
  return(data.frame(y = y, g, g.0, ind))
  
}

#' Simulate data according to  Strobl et al. (2007) simulation design
#'
#' @param n Numeric, the number of samples to be simulated.
#' @param relevance Numeric used to specify the probability of success in 
#' simulating the outcome.
#'
#' @returns Tibble with simulated data.
strobl_sim <- function(n = 100, relevance = 0.1) {
  X1 <- rnorm(n, mean = 0, sd = 1)
  
  X2 <- as.factor(sample(x = letters[1:2],
                         size = n,
                         replace = T))
  
  X3 <- as.factor(sample(x = letters[1:4],
                         size = n,
                         replace = T))
  
  X4 <- as.factor(sample(x = letters[1:10],
                         size = n,
                         replace = T))
  
  X5 <- as.factor(sample(x = apply(expand.grid(letters, letters), 1, paste, collapse="")[1:20],
                         size = n,
                         replace = T))
  
  sim <- data.frame(X1, X2, X3, X4, X5)
  
  sim <- sim %>% rowwise() %>% mutate(y = power_case(X2, relevance = relevance))
  return(sim)
}

#' Helper function for `strobl_sim()`, simulates outcome variable.
#'
#' @param test_object Data frame with simulated predictor variables.
#' @param test Character specifying the failure.
#' @param relevance Numeric, a value between 0 and 0.5 that determines how much
#'   the base 50% success probability is shifted.
#' @param verbose Boolean, specifies whether used probability should be printed.
#' @returns Integer (0 or 1) - simulated outcome variable.
power_case <- function(test_object, test = "a", relevance = 0, verbose = F) {
  if(relevance>0 & verbose ==T) {cat(ifelse(test_object == test, 0.5 - relevance, 0.5 + relevance))}
  rand <- rbinom(n=1, size=1, prob = ifelse(test_object == test, 0.5 - relevance, 0.5 + relevance))
  return(rand)
}

