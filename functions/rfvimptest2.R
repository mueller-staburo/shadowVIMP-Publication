#adapted rfvimptest function to allow setting respect.unordered.factors to 'order' in the ranger call. 
#Official version at the time of the simulation study did not allow passing ... arguments to ranger due to a bug.
#Only difference is in line 48, adding of the parameter to the ranger call.
rfvimptest2 <- function(data, yname, Mmax = 500, varnames = NULL, p0 = 0.06, p1 = 0.04, alpha = 0.05, beta = 0.2, A = 0.1, B = 10, h = 8, nperm = 1,
                       ntree = 500,
                       progressbar = TRUE,
                       num.threads = 5, 
                       test = c("general", "twosample")[1],
                       type = c("SPRT", "SAPT", "pval", "certain", "complete")[1], condinf=FALSE, ...) {
  starttime <- Sys.time()
  # @seealso \code{\link{predict.divfor}}
  
  if(any(is.na(data))) {
    missvariables <- paste(names(data)[apply(data, 2, function(x) any(is.na(x)))], collapse = ", ")
    stop(paste0("Missing data in columns: ", missvariables, ". Please provide complete data or consider setting condinf=TRUE."))
  }
  
  if(!condinf & test == "twosample")
    stop("'twosample' approach only useable if condinf=TRUE.")
  
  if(!condinf & nperm > 1)
    stop("Values of 'nperm' different than 1 are only useable if condinf=TRUE.")
  
  if (progressbar) pb <- txtProgressBar(min = 1, max = Mmax, initial = 1, width = 10, style = 3, char = "|")
  
  if (type == "SPRT") {
    A <- beta / (1 - alpha)
    B <- (1 - beta) / alpha
  }
  if (type %in% c("SPRT", "SAPT")) {
    logA <- log(A)
    logB <- log(B)
    help1 <- log((1 - p0) / (1 - p1))
    help2 <- log((p1 * (1 - p0)) / (p0 * (1 - p1)))
  }
  
  stop_crits <- switch(type,
                       SPRT = list((logA + 1:Mmax * help1) / help2, (logB + 1:Mmax * help1) / help2),
                       SAPT = list((logA + 1:Mmax * help1) / help2, (logB + 1:Mmax * help1) / help2),
                       pval = list(rep(h, times = Mmax), rep(h, times = Mmax)),
                       certain = list(rep(alpha*Mmax, times = Mmax), Mmax*alpha - Mmax + 1:Mmax),
                       complete = NULL)
  
  if (is.null(varnames))
    varnames <- names(data)[names(data)!=yname]
  
  if (!condinf) {
    rfmod <- ranger::ranger(data = data, dependent.variable.name=yname, num.tree=ntree, num.threads = num.threads, importance = "permutation", respect.unordered.factors = "order")
    vimp_orig <- list()
    vimp_orig$values <- rfmod$variable.importance[varnames]
  }
  else {
    rfmod <- party::cforest(as.formula(paste(yname, " ~ .", sep="")), data = data, controls = party::cforest_unbiased(ntree=ntree, ...))
    vimp_orig <- permimp::permimp(rfmod, whichxnames = varnames, nperm = nperm, asParty = TRUE, progressBar = FALSE)
  }
  
  
  if (test == "general") {
    testresult <-
      lapply(varnames, function(v) {
        permdata <- data
        permvimps <- c()
        for (m in 1:Mmax) {
          if (progressbar) {setTxtProgressBar(pb, m)}
          if (m == 1 & progressbar) cat(" of variable", v)
          permdata[, v] <- sample(permdata[, v])
          
          if (!condinf) {
            permmod <- ranger::ranger(data = permdata, dependent.variable.name=yname, num.tree=ntree, num.threads = num.threads, importance="permutation", respect.unordered.factors = "order")
            permvimps <- c(permvimps, permmod$variable.importance[v])
          }
          else {
            permmod <- party::cforest(as.formula(paste(yname, " ~ .", sep="")), data = permdata, controls = party::cforest_unbiased(ntree=ntree, ...))
            permvimps <- c(permvimps, permimp::permimp(permmod, whichxnames = v, nperm = nperm, asParty = TRUE, progressBar = FALSE)$values)
          }
          d <- sum(permvimps >= vimp_orig$values[v])
          if (type == "certain") {
            if (d > stop_crits[[1]][m]) {result <- "keep H0"; pvalue <- NA; break} else if (d <= stop_crits[[2]][m]) {result <- "accept H1"; pvalue <- NA; break}
          } else if (type %in% c("SPRT", "SAPT")) {
            if (d >= stop_crits[[1]][m]) {result <- "keep H0"; pvalue <- NA; break} else if (d <= stop_crits[[2]][m]) {result <- "accept H1"; pvalue <- NA; break}
          } else if (type == "pval") {
            if (d == h) {
              pvalue <- d/m
              result <- ifelse(pvalue > 0.05, "keep H0", "accept H1")
              break
            }
          }
        }
        if (m == Mmax) {
          if (type == "pval") {
            if (d < h) {
              pvalue <- (d + 1) / (Mmax + 1)
              result <- ifelse(pvalue > 0.05, "keep H0", "accept H1")
            }
            else {
              pvalue <- d / Mmax
              result <- ifelse(pvalue > 0.05, "keep H0", "accept H1")
            }
          } else  if (type == "complete") {
            pvalue <- d / Mmax
            result <- ifelse(pvalue > 0.05, "keep H0", "accept H1")
          } else {
            pvalue <- NA
            result <- ifelse(d / Mmax > 0.05, "keep H0", "accept H1")
          }
        }
        if (progressbar) cat(" - finished", "\n")
        list(testres=result, pvalue=pvalue, stoppedearly=ifelse(m < Mmax, "yes", "no"), permperf=m)
      })
  } else if (test == "twosample") {
    testresult <-
      lapply(varnames, function(v) {
        permdata <- data
        permdata[, v] <- sample(permdata[, v])
        permmod <- party::cforest(as.formula(paste(yname, " ~ .", sep="")), data = permdata, controls = party::cforest_unbiased(ntree=ntree, ...))
        permmodvimps <- permimp::permimp(permmod, whichxnames = v, nperm = nperm, asParty = TRUE, progressBar = FALSE)
        permvimps <- c()
        for (m in 1:Mmax) {
          if (progressbar) {setTxtProgressBar(pb, m)}
          if (m == 1 & progressbar) cat(" of variable", v)
          permvimps <- c(permvimps, mean(c(vimp_orig$perTree[, v], permmodvimps$perTree[, v])[sample(1:(2*ntree), ntree)]))
          d <- sum(permvimps >= vimp_orig$values[v])
          if (type == "certain") {
            if (d > stop_crits[[1]][m]) {result <- "keep H0"; pvalue <- NA; break} else if (d <= stop_crits[[2]][m]) {result <- "accept H1"; pvalue <- NA; break}
          } else if (type %in% c("SPRT", "SAPT")) {
            if (d >= stop_crits[[1]][m]) {result <- "keep H0"; pvalue <- NA; break} else if (d <= stop_crits[[2]][m]) {result <- "accept H1"; pvalue <- NA; break}
          } else if (type == "pval") {
            if (d == h) {
              pvalue <- d/m
              result <- ifelse(pvalue > 0.05, "keep H0", "accept H1")
              break
            }
          }
        }
        if (m == Mmax) {
          if (type == "pval") {
            if (d < h) {
              pvalue <- (d + 1) / (Mmax + 1)
              result <- ifelse(pvalue > 0.05, "keep H0", "accept H1")
            }
            else {
              pvalue <- d / Mmax
              result <- ifelse(pvalue > 0.05, "keep H0", "accept H1")
            }
          } else  if (type == "complete") {
            pvalue <- d / Mmax
            result <- ifelse(pvalue > 0.05, "keep H0", "accept H1")
          } else {
            pvalue <- NA
            result <- ifelse(d / Mmax > 0.05, "keep H0", "accept H1")
          }
        }
        if (progressbar) cat(" - finished", "\n")
        list(testres=result, pvalue=pvalue, stoppedearly=ifelse(m < Mmax, "yes", "no"), permperf=m)
      })
  }
  stoptime <- Sys.time()
  time_elapsed <- paste0(round(as.numeric(difftime(stoptime, starttime, units = "secs")), 1), " seconds")
  testres <- sapply(testresult, function(x) x$testres)
  pvalues <- sapply(testresult, function(x) x$pvalue)
  stoppedearly <- sapply(testresult, function(x) x$stoppedearly)
  perms <- sapply(testresult, function(x) x$permperf)
  names(testres) <- names(pvalues) <- names(stoppedearly) <- names(perms) <- names(vimp_orig$values)
  
  result <- list(testtype = paste(test, type, sep=", "), varimp=vimp_orig$values, testres = testres, pvalues = pvalues,
                 stoppedearly = stoppedearly, perms = perms, Mmax=Mmax, ntree=ntree, comptime = time_elapsed)
  
  class(result) <- "rfvimptest"
  
  return(result)
  
}
