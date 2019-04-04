#' predict makes a PredictionResponse for Jaqpot
#' @param dataset
#' @param rawModel
#' @param additionalInfo


predict.base.lm.glm <- function(dataset, rawModel, additionalInfo){
  # Get feature keys (a key number that points to the url)
  feat.keys <-  dataset$features$key
  # Get feature names (actual name)
  feat.names <- dataset$features$name
  # Create a dataframe that includes the feature key and the corresponding name
  key.match <- data.frame(cbind(feat.keys, feat.names))
  # Convert factor to string (feat.names is converted factor by data.frame())
  key.match[] <- lapply(key.match, as.character)
  # Initialize a dataframe with as many rows as the number of values per feature
  rows_data <- length(dataset$dataEntry$values[,2])
  df <- data.frame(matrix(0, ncol = 0, nrow = rows_data))
 
  for(key in feat.keys){
    # For each key (feature) get the vector of values (of length 'row_data')
    feval <- dataset$dataEntry$values[key][,1]
    # Name the column with the corresponding name that is connected with the key
    df[key.match[key.match$feat.keys == key, 2]] <- feval
  }
  # Unserialize the model
  mod <- unserialize(base64_dec(rawModel))
  model <- mod$MODEL
  # Extract the predicted value names
  predFeat <- additionalInfo$predictedFeatures[1][[1]]
  # Make the prediction using the model and the new data
  # Note that the names of the dataframe must be the same with the original
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
