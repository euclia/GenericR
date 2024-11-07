# helpers/parallel.R

library(future)
library(promises)

WORKERS <- strtoi(Sys.getenv("WORKERS", 3))

future::plan(future::multisession(workers = WORKERS))

