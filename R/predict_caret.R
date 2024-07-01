#' predict makes a PredictionResponse for Jaqpot
predict.caret <- function(modelDto, datasetDto, additionalInfo, rawModel, doa) {

  #################################
  ## Input retrieval from Jaqpot ##
  #################################

  # Get feature names (actual name)
  feat.names <- modelDto$independentFeatures$name
  # Get feature types among FLOAT, INTEGER, STRING, TEXT, SMILES
  feat.types <-  modelDto$independentFeatures$featureType
  names(feat.types) <- feat.names
  # Get input values
  df = datasetDto$input

  # Convert data types
  for (j in 1:dim(df)[2]){
    if (feat.types[colnames(df)[j]] == "FLOAT"){
      df[,j] <- as.numeric( df[,j] )
    }else if (feat.types[colnames(df)[j]] == "INTEGER"){
      df[,j] <- as.integer( df[,j] )
    }else{
      # We don't need to do any conversion from STRING/ CATEGORICAL/ TEXT
      # as the input is already in a string format
    }
  }

  # Convert "NA" to NA
  for (i in 1:dim(df)[1]) {
    for (j in 1:dim(df)[2]) {
      if (!is.na(df[i, j])) {
        if (df[i, j] == "NA") {
          df[i, j] <- NA
        }
      }
    }
  }


  # Extract the predicted value names
  predFeat <-modelDto$dependentFeatures$name
  # Make the prediction using the model and the new data
  # Note that the names of the dataframe must be the same with the original


  ###########################
  ## Model unserialization ##
  ###########################

  decoded_data <- jsonlite::base64_dec(rawModel)
  mod <- unserialize(decoded_data)
  model <- mod$MODEL
  preprocess <- mod$PREPROCESS
  ensemble <- mod$ENSEMBLE
  if (length(mod$extra.args) == 1) {
    extra.args <- mod$extra.args[[1]]
  } else {
    extra.args <- NULL
  }
  # Replace NAs
  replace <- additionalInfo$fromUser$replace
  if (!is.null(replace)) {
      replace.position <- names(replace)
      replace.value <- as.numeric(replace[[1]])
  }
  ####################
  ## Preprocessing ##
  ####################
  # Do the NA substitution before preprocessing, if "before" is provided by the user
  if (!is.null(replace)) {
    if (replace.position== "before") {
      for (i in 1:dim(df)[1]) {
        for (j in 1:dim(df)[2]) {
          if (is.na(df[i, j])) {
            df[i, j] <- replace.value
          }
        }
      }
    }
  }

  # If there is a preprocess stage apply it, else just predict
  if (!is.null(preprocess)) {

    #if there is a preprocess model that is dummy vars, then retrieve factors
    ModelForNames = NULL
    for (k in 1:length(preprocess)) {
      if (attributes((preprocess[[k]]))$class == "dummyVars") {
        ModelForNames <- preprocess[[k]]
        # Retrive the original classes of the dataset's categorical vars
        for (j in 1:dim(df)[2]) {
          #Retrieve levels of factor
          if (attr(ModelForNames$terms, "dataClasses")[colnames(df)[j]] == "factor") {
            df[, j] <- factor(df[, j], levels = ModelForNames$lvls[colnames(df)[j]][[1]])
          }
        }
      }
    }

    # If there is no dummy vars, use the first model to retrieve factors
    if (is.null(ModelForNames)) {
      # If it's ensemble, there are multiple models, so use one for getting the categorical variables
      if (!is.null(ensemble)) {
        ModelForNames <- model[[1]]
      }else {
        ModelForNames <- model
      }
      # Retrive the original classes of the dataset's categorical vars
      for (j in 1:dim(df)[2]) {
        #Retrieve levels of factor
        if (attr(ModelForNames$terms, "dataClasses")[colnames(df)[j]] == "factor") {
          df[, j] <- factor(df[, j], levels = ModelForNames$xlevels[colnames(df)[j]][[1]])
        }
      }
    }

    #Data preprocessing
    for (p in 1:length(preprocess)) {
      preprocess.method <- preprocess[[p]]
      preprocessData <- predict(preprocess.method, df)
      df <- preprocessData
    }

  }


  # Do the NA substitution after preprocessing, if "after" is provided by the user
  if (!is.null(replace)) {
    if (replace.position== "after") {
      for (i in 1:dim(df)[1]) {
        for (j in 1:dim(df)[2]) {
          if (is.na(df[i, j])) {
            df[i, j] <- replace.value
          }
        }
      }
    }
  }

  ################
  ## Prediction ##
  ################

  # Retrieve ensemble column names
  ensemble.vars <- additionalInfo$fromUser$ensemble.vars

  if (!is.null(ensemble)) {
    stacking <- matrix(rep(NA, length(model) * dim(df)[1]), ncol = length(model))
    for (i in 1:length(model)) {
      # Select the model to use
      UseModel <- model[[i]]
      stacking[, i] <- predict(UseModel, df)
    }
    # Convert the stacked predictions matrix to dataframe and name the columns of the dataset appropriately
    stacking <- as.data.frame(stacking)
    colnames(stacking) <- ensemble.vars
    # Predict using the ensemble model
    predictions <- predict(ensemble, stacking)
  }else {
    predictions <- predict(model, df)
  }


  ######################
  ## Detransformation ##
  ######################

  #Apply detransformation
  ymin <- additionalInfo$fromUser$ymin
  ymax <- additionalInfo$fromUser$ymax
  if (length(ymin) == 1 && length(ymax) == 1) {
    for (i in 1:length(predictions)) {
      predictions[i] <- predictions[i] * (ymax - ymin) + ymin
    }
  }

  # Not offered to users (custom detransformation)
  if (!is.null(extra.args)) {
    predictions <- extra.args(predictions)
  }

  ##################################
  ## Name and return predictions  ##
  ##################################

  for (i in 1:length(predictions)) {
    prediction <- data.frame(predictions[i])
    colnames(prediction) <- predFeat
    if (i == 1) { lh_preds <- list(jsonlite::unbox(prediction))
    }else {
      lh_preds[[i]] <- jsonlite::unbox(prediction)
    }
  }
  datpred <- list(predictions = lh_preds)
  return(datpred)
}
