bootstrapf <- function(vector_to_boot) {
  ret<- list(setNames(unlist(Hmisc::smean.cl.boot(vector_to_boot, B=100000)), c("mean", "lower", "upper")))
  return(ret)
}

