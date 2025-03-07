# source("simulation_degenhardt.R")
# 
# data <- simulation.data.cor(no.samples = 100, group.size = rep(10, 6), no.var.total = 200)

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

# res1 <- boruta_wrapper(data=data)
# res1$decis
# res1$finalDecision %>% names()
