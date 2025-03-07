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