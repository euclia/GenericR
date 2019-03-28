#' genericquickpredict takes a quickPredictResponse and returns prediction
#' @param independentFeatures
#' @param rawModel
#' @param implmentedWith

genericquickpredict <- function(independentFeatures, rawModel, implmentedWith){
  mod <- unserialize(base64_dec(rawModel))
  fdf <- as.data.frame(independentFeatures)
  pred <- list(prediction=prediction)
  return(pred)
}
