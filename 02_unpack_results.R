library(dplyr)
library(foreach)
library(stringr)
lapply(list.files(path = "functions/", full.names= T), source)
set.seed(1807)
seed_list <- floor(runif(1000, min = 1, max = 999999))

all_files <- list.files(pattern = "_small\\.rds$", full.names = TRUE, recursive = TRUE)

# Create a list of files that include the word "proposed" in their names
proposed_files <- all_files[grepl("proposed", all_files)]

# Create a list of files that exclude the word "proposed" in their names
non_proposed_files <- all_files[!grepl("proposed", all_files)]

# Print the lists
print(proposed_files)
print(non_proposed_files)

# Read each .rds file into a list
data_list_proposed <- lapply(proposed_files, readRDS)
names(data_list_proposed) <- proposed_files
data_list_others <- lapply(non_proposed_files, readRDS)
names(data_list_others) <- non_proposed_files

#Creates perf_others.xlsx which contains all performance measures for methods other than shadowVIMP

perf_others <- foreach(i=1:length(data_list_others), .combine=rbind) %do% {
performance_others(data_list_others[[i]]) %>% group_by(identifier, type, iterprop, correction) %>% summarise_all(tfl_output)
}%>% mutate(design = case_when(
  str_detect(identifier, regex("fried", ignore_case = TRUE)) ~ "Friedman",
  str_detect(identifier, regex("strobl", ignore_case = TRUE)) ~ "Strobl",
  str_detect(identifier, regex("deg_50", ignore_case = TRUE)) ~ "Degenhardt Group Size 50",
  str_detect(identifier, regex("deg_10", ignore_case = TRUE)) ~ "Degenhardt Group Size 10",
  str_detect(identifier, regex("nicodemus", ignore_case = TRUE)) ~ "Nicodemus Null Design",
  TRUE ~ "Other"  # Default case
)) %>% mutate(ntree = case_when(
  str_detect(identifier, regex("10000", ignore_case = TRUE)) ~ "10000",
  str_detect(identifier, regex("500", ignore_case = TRUE)) ~ "500",
  TRUE ~ "10000"  # Default case same as 10000
))%>%
  mutate(Method = case_when(
    str_detect(identifier, regex("boruta", ignore_case = TRUE)) ~ "Boruta",
    str_detect(identifier, regex("vita", ignore_case = TRUE)) ~ "Vita",
    str_detect(identifier, regex("rfvimptest", ignore_case = TRUE)) ~ "rfvimptest",
    TRUE ~ "Other"  # Default case if no match
  ))

perf_others %>% writexl::write_xlsx(path="perf_others.xlsx")


#Creates perf_proposed.xlsx which contains all performance measures for the (proposed) method shadowVIMP

perf_proposed <- foreach(typ = c("per_variable", "pooled"), .combine = rbind) %:%
  foreach(corre = c("with_correction", "without_correction"), .combine = rbind) %:%
  foreach(iterpr = c("0_2", "0_4", "0_6", "0_8", "1"), .combine = rbind) %:% 
            foreach(i=1:length(data_list_proposed), .combine=rbind) %do% {
  performance_proposed(data_list_proposed[[i]], correction = corre, type=typ, iterprop = iterpr) %>% group_by(identifier, type, iterprop, correction)  %>% summarise_all(tfl_output)
            } %>% mutate(design = case_when(
              str_detect(identifier, regex("fried", ignore_case = TRUE)) ~ "Friedman",
              str_detect(identifier, regex("strobl", ignore_case = TRUE)) ~ "Strobl",
              str_detect(identifier, regex("deg_50", ignore_case = TRUE)) ~ "Degenhardt Group Size 50",
              str_detect(identifier, regex("deg_10", ignore_case = TRUE)) ~ "Degenhardt Group Size 10",
              str_detect(identifier, regex("nicodemus", ignore_case = TRUE)) ~ "Nicodemus Null Design",
              TRUE ~ "Other"  # Default case
            )) %>%
  mutate(Method = case_when(
    str_detect(identifier, regex("with_preselect", ignore_case = TRUE)) ~ "Proposed with preselect",
    str_detect(identifier, regex("without_preselect", ignore_case = TRUE)) ~ "Proposed without preselect"))


perf_proposed %>% writexl::write_xlsx(path="perf_proposed_all.xlsx")



## Nicodemus design selection by variable investigation

#select other
#selection all variable names as header
var_names <- unique(c(names(nicodemeus_sim()),
names(simulation.data.cor(no.samples=100, group.size=rep(50, 6), no.var.total = 5000)),
names(simulation.data.cor(no.samples=100, group.size=rep(10, 6), no.var.total = 5000)),
names(data.frame(mlbench::mlbench.friedman1(n = 100))),
names(strobl_sim())))

empty_df_with_var_names <- setNames(data.frame(matrix(ncol = length(var_names), nrow = 0)), var_names)


#Selections of methods not shadowVIMP
select_others_raw <- lapply(data_list_others, selections_others)
select_others <- data.table::rbindlist(list(data.table::rbindlist(select_others_raw, fill =T)%>% mutate(design = case_when(
  str_detect(identifier, regex("fried", ignore_case = TRUE)) ~ "Friedman",
  str_detect(identifier, regex("strobl", ignore_case = TRUE)) ~ "Strobl",
  str_detect(identifier, regex("deg_50", ignore_case = TRUE)) ~ "Degenhardt Group Size 50",
  str_detect(identifier, regex("deg_10", ignore_case = TRUE)) ~ "Degenhardt Group Size 10",
  str_detect(identifier, regex("nicodemus", ignore_case = TRUE)) ~ "Nicodemus Null Design",
  TRUE ~ "Other"  # Default case
), .before = everything()) %>% mutate(ntree = case_when(
  str_detect(identifier, regex("10000", ignore_case = TRUE)) ~ "10000",
  str_detect(identifier, regex("500", ignore_case = TRUE)) ~ "500",
  TRUE ~ "10000"  # Default case same as 10000
),.before = everything())%>%
  mutate(Method = case_when(
    str_detect(identifier, regex("boruta", ignore_case = TRUE)) ~ "Boruta",
    str_detect(identifier, regex("vita", ignore_case = TRUE)) ~ "Vita",
    str_detect(identifier, regex("rfvimptest", ignore_case = TRUE)) ~ "rfvimptest",
    TRUE ~ "Other"  # Default case if no match
  ),.before = everything()), empty_df_with_var_names), fill=T)

select_others_nico <- select_others %>% filter(design == "Nicodemus Null Design") %>% select_if(~ !all(is.na(.)))


#Selections of (proposed) method shadowVIMP
select_proposed_raw <- c(lapply(data_list_proposed, selections_proposed, iterprop = "1", type = "pooled", correction = "with_correction"),
                            lapply(data_list_proposed, selections_proposed, iterprop = "1", type = "per_variable", correction = "with_correction"))
select_proposed <- data.table::rbindlist(list(data.table::rbindlist(select_proposed_raw, fill =T)%>% mutate(design = case_when(
  str_detect(identifier, regex("fried", ignore_case = TRUE)) ~ "Friedman",
  str_detect(identifier, regex("strobl", ignore_case = TRUE)) ~ "Strobl",
  str_detect(identifier, regex("deg_50", ignore_case = TRUE)) ~ "Degenhardt Group Size 50",
  str_detect(identifier, regex("deg_10", ignore_case = TRUE)) ~ "Degenhardt Group Size 10",
  str_detect(identifier, regex("nicodemus", ignore_case = TRUE)) ~ "Nicodemus Null Design",
  TRUE ~ "Other"  # Default case
), .before = everything()) %>% mutate(ntree = case_when(
  str_detect(identifier, regex("10000", ignore_case = TRUE)) ~ "10000",
  str_detect(identifier, regex("500", ignore_case = TRUE)) ~ "500",
  TRUE ~ "10000"  # Default case same as 10000
),.before = everything())%>%
  mutate(Method = case_when(
    str_detect(identifier, regex("with_preselect", ignore_case = TRUE)) ~ "Proposed with preselect",
    str_detect(identifier, regex("without_preselect", ignore_case = TRUE)) ~ "Proposed without preselect")
    ,.before = everything()), empty_df_with_var_names), fill=T)

select_proposed_nico <- select_proposed %>% filter(design == "Nicodemus Null Design" & adjustment == "unadjusted") %>% select_if(~ !all(is.na(.)))


data.table::rbindlist(list(select_others_nico, select_proposed_nico), fill = T ) %>% 
  filter(ntree==10000 & adjustment == "unadjusted") %>% writexl::write_xlsx("nico_type1_error.xlsx")

data.table::rbindlist(list(select_others_nico, select_proposed_nico), fill = T ) %>% 
  filter(ntree==10000 & adjustment == "unadjusted") %>% 
  select(Method, type, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12) %>% 
  rename_with(~ gsub("^V", "X", .), starts_with("V")) %>% 
  writexl::write_xlsx("nico_type1_error.xlsx")
