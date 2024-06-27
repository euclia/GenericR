library(plumber)

parsers = list(json = list(simplifyVector = FALSE), rds = list())

# 'plumber.R' is the location of the file shown above
pr("predict.R") %>%
  pr_set_parsers(parsers) %>%
  pr_run(port=8004)

