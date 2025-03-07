add_degenhardt_informative_status <- function(group_size = 50) {
vars <- names(simulation.data.cor(100, rep(group_size, 6),  5000))
return(vars[stringr::str_which(vars, "^g.1|^g.2|^g.3")])
}
