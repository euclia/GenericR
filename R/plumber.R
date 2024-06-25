library(plumber)
# 'plumber.R' is the location of the file shown above
pr("predict.R") %>%
  pr_run(port=8004)
