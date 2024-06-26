# R/plumber.R

library(plumber)

# Load the predict function
source("predict_pbpk.R")
source("predict_caret.R")

#* @post /predict.pbpk
#* @param dataset
#* @param model
#* @param additionalInfo
#* @param doa
function(model, dataset, additionalInfo, rawModel, doa) {
  predict.pbpk(model, dataset, additionalInfo, rawModel, doa)
}

#* @post /predict.caret
#* @param dataset
#* @param model
#* @param additionalInfo
#* @param rawModel
function(model, dataset, additionalInfo, rawModel, doa) {
  predict.caret(model, dataset, additionalInfo, rawModel, doa)
}

#* @get /health
function() {
  list(status = "OK")
}

