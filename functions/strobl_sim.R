power_case <- function(test_object, test = "a", relevance = 0, verbose = F) {
  if(relevance>0 & verbose ==T) {cat(ifelse(test_object == test, 0.5 - relevance, 0.5 + relevance))}
  rand <- rbinom(n=1, size=1, prob = ifelse(test_object == test, 0.5 - relevance, 0.5 + relevance))
  return(rand)
}

strobl_sim <- function(n = 100, relevance = 0.1) {
  
  n = n
  
  
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