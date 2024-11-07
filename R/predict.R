# R/plumber.R

alive <<- TRUE

library(plumber)

parser_jaqpot <- function(...) {
  function(value, content_type = "application/json", ...) {
    value <- rawToChar(value)
    value <- jsonlite::fromJSON(value, simplifyVector = TRUE)
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
function(model, dataset) {
  future_promise({
    return(predict.pbpk(model, dataset))
  })
}

#* @post /predict_caret
#* @param dataset
#* @param model
#* @param additionalInfo
#* @param rawModel
#* @parser parser_jaqpot
function(model, dataset) {
  future_promise({
    return(predict.caret(model, dataset))
  })

}

#* @get /health
function() {
  list(status = "OK")
}


#* Health check. Returns "OK".
#* @serializer text
#* @get /health
function() {
  future({
    if (!alive) stop() else "OK"
  })
}
