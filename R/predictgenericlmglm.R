#' predict makes a PredictionResponse for Jaqpot
#' @param dataset
#' @param rawModel
#' @param additionalInfo


PredictBaseLmGlm <- function(dataset, rawModel, additionalInfo){
  feat.keys <-  dataset$features$key
  feat.names <- dataset$features$names
  feat.names <- as.vector(unlist(lapply(feat.names, as.character)))
  key.match <- data.frame(cbind(feat.keys, feat.names), stringAsFactors = FALSE)
  
  rows_data <- length(dataset$dataEntry$values[,2])
  df <- data.frame(matrix(0, ncol = 0, nrow = rows_data))

  for(key in feat.keys){
    feval <- dataset$dataEntry$values[i][,1]
    df[key.match[key.match$feat.keys == i, 2]] <- feval
  }
  mod <- unserialize(base64_dec(rawModel))
  model <- mod$MODEL
  predFeat <- additionalInfo$predictedFeatures[1][[1]]
  predictions <- predict(model, df)
  for(i in 1:length(predictions)){
    prediction<- data.frame(predictions[i])
    colnames(prediction)<- predFeat
    if(i==1){lh_preds<- list(unbox(prediction))
    }else{
      lh_preds[[i]]<- unbox(prediction)
    }
  }
  datpred <-list(predictions=lh_preds)
  return(datpred)
}
