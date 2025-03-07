summary_statistics_pipe <- function(df, variable) {
  summarise(df, across(all_of(variable), 
                   .fns = 
                     list(min = min,
                          median = median,
                          mean = mean,
                          stdev = sd,
                          q25 = ~quantile(., 0.25),
                          q75 = ~quantile(., 0.75),
                          max = max),
                   .names = "{.fn}")) %>%
    tidyr::pivot_longer(cols = -c(pooled), names_to = "measure", values_to = variable)
}