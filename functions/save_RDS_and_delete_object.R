# Save the object in the selected directory and remove it from the global environment
save_RDS_and_delete_object <- function(object_to_save,
                                       filename = paste0(deparse(substitute(object_to_save)), ".rds"),
                                       subfolder = "rds", 
                                       delete_object = T) {
  if(subfolder!="" & !dir.exists(subfolder)) dir.create(subfolder)
  saveRDS(object_to_save, file = paste0(ifelse(subfolder!="", paste0(subfolder, "/"), ""), filename))
  if(delete_object) {rm(list=as.character(substitute(object_to_save)), envir = .GlobalEnv)}
}
