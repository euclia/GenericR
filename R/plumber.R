library(plumber)

pr("predict.R") %>%
  pr_run(port=8004)

