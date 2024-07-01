# R/plumber.R

library(plumber)


parser_jaqpot <- function(...) {
  function(value, content_type = "application/json", ...) {
    value <- rawToChar(value)
    value <- jsonlite::fromJSON(value, simplifyVector = FALSE)
  }
}

# Register the newly created parser
register_parser("parser_jaqpot", parser_jaqpot, fixed = "application/json")

# Load the predict function
source("predict_pbpk.R")
source("predict_caret.R")

#* @post /predict_pbpk
#* @param dataset
#* @param model
#* @param additionalInfo
#* @param doa
#* @parser parser_jaqpot
function(model, dataset, additionalInfo, rawModel, doa) {
  predict.pbpk(model, dataset, additionalInfo, rawModel, doa)
}

#* @post /predict_caret
#* @param dataset
#* @param model
#* @param additionalInfo
#* @param rawModel
#* @parser parser_jaqpot
function(model, dataset, additionalInfo, rawModel, doa) {
  predict.caret(model, dataset, additionalInfo, rawModel, doa)
}

#* @get /health
function() {
  list(status = "OK")
}

