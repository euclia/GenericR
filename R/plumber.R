library(plumber)

pr("predict.R") %>%
  pr_run(host=0.0.0.0,port=8004)

