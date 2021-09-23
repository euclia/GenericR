#' jaqpot.predict.bnlearn.discrete makes a PredictionResponse for Jaqpot for a discrete bayesian model created using package 'bnlearn'
#' @param dataset The input from the user along with the feature names.
#' @param rawModel The serialised model.
#' @param additionalInfo additional information from user e.g. prediction method specification.
#'

jaqpot.predict.bnlearn.discrete <- function(dataset, rawModel, additionalInfo){
  
  #################################
  ## Input retrieval from Jaqpot ##
  #################################
  
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
  
  # Convert "NA" to NA
  for (i in 1:dim(df)[1]){
    for (j in 1:dim(df)[2]){
      if(!is.na(df[i,j])){
        if(df[i,j] == "NA"){
          df[i,j] <- NA
        }
      }
    }
  }
  
  # Extract the predicted value names
  predicted.feats <- rep(0,  length(additionalInfo$predictedFeatures))
  for (i in 1:length(predicted.feats)){
    predicted.feats[i] <- additionalInfo$predictedFeatures[[i]]
  }
  
  ###########################
  ## Model unserialization ##
  ###########################
  
  mod <- unserialize(jsonlite::base64_dec(rawModel))
  model <- mod$MODEL
  method <- additionalInfo$fromUser$predict.args$method
  n <- additionalInfo$fromUser$predict.args$n
  
    # Convert categorical variables to factors
    for (i in 1:dim(df)[2]){
      varname <- names(df)[i]
      if(varname !=  "query node"){
        # Retrieve the levels from the model and convert to factor
        df[,i] <- factor(df[,i], levels = attributes(model[[eval(expr(varname))]]$prob)$dimnames[[1]])
      }
   }
  
  ################
  ## Prediction ##
  ################
  
  #Define a parameter to store thenumber of levels of each query nodes
  level.count <- rep(NA,dim(df)[1] )
  # Get output dimensions depending on the number of levels of each query node
  for (instance in 1:dim(df)[1]){
    pred.node <- as.character(df$`query node`[instance])
    level.count[instance] <- length(levels(df[[eval(expr(pred.node))]]))
  }

  # Create a matrix to store the solution
  prediction <- matrix(rep(NA,3*sum(level.count)), ncol = 3)
  # Create a variable to store the last row filled in the prediction matrix
  where <- 1
 for (instance in 1:dim(df)[1]){
   pred.node <- as.character(df$`query node`[instance])
   #Provide only non NA values
   newdata <-  df[instance,!is.na(df[instance,])]
   # Remove the query node
   newdata <-  newdata[ ,-which(names(newdata) == "query node")]
   # Store in the first column the query node
   prediction[where:(where+level.count[instance]-1),1] <- pred.node
   # Make the prediction
   result = attributes(predict(model, data = newdata, node= pred.node, prob = T, method = method, n = n))$prob[,1]
   # Store in the seconde column the levels
   prediction[where:(where+level.count[instance]-1),2] <- names(result)
   # Store the probabilities returned by the model in the third column
   prediction[where:(where+level.count[instance]-1),3] <- round(result,3)
   #Update the row count
   where <- where+level.count[instance]
 }
  colnames(prediction) <- c("query node", "prediction class", "probability")
  
  ##################################
  ## Name and return predictions  ##
  ##################################
  for(i in 1:dim(prediction)[1]){
    pred<- data.frame((t(prediction[i,])))
    # Bring everything into a format that cooperates with Jaqpot
    if(i==1){lh_preds<- list(jsonlite::unbox(pred))
    }else{
      lh_preds[[i]]<- jsonlite::unbox(pred)
    }
  }
  datpred <-list(predictions=lh_preds)
  
  return(datpred)
}
