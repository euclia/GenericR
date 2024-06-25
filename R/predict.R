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
function(dataset, model, additionalInfo, doa) {
  predict.pbpk(dataset, model, additionalInfo, doa)
}

#* @post /predict.caret
#* @param dataset
#* @param model
#* @param additionalInfo
#* @param doa
function(dataset, model, additionalInfo, doa) {
  predict.caret(dataset, model, additionalInfo, doa)
}

#* @get /health
function() {
  list(status = "OK")
}

