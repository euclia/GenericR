#' predict makes a PredictionResponse for Jaqpot
#' @param datasetDto
#' @param rawModel
#' @param additionalInfo
#'

predict.caret <- function(datasetDto, modelDto, additionalInfo, doa) {

  #################################
  ## Input retrieval from Jaqpot ##
  #################################

  # Get feature names (actual name)
  feat.names <- modelDto$independentFeatures$name
  if (class(datasetDto$input$values[[1]]) %in% c("matrix", "array" )){
    # Initialize a dataframe with as many rows as the number of values per feature
    rows <- dim(datasetDto$input$values[[1]])[1]
    cols <- dim(datasetDto$input$values[[1]])[2]
    df <- data.frame(matrix(NA, ncol = cols, nrow = rows))
    colnames(df) <- feat.names

    for (row in 1:rows) {
      for (column in 1:cols){
        df[row, column] <- datasetDto$input$values[[1]][row,column]
      }
    }

  }else{
    df <- data.frame(matrix(datasetDto$input$values[[1]], ncol = length(feat.names), nrow = 1))
    colnames(df) <- feat.names
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
  predFeat <- additionalInfo$predictedFeatures[1][[1]]
  # Make the prediction using the model and the new data
  # Note that the names of the dataframe must be the same with the original


  ###########################
  ## Model unserialization ##
  ###########################
  print(class(modelDto$actualModel))

  decoded_data <- jsonlite::base64_dec(modelDto$actualModel[[1]])
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
    #Convert character to numeric
    if (!is.na(as.numeric(replace[2]))) {
      replace.value <- as.numeric(replace[2])
    }else {
      replace.value <- replace[2]
    }
  }
  ####################
  ## Preprocessing ##
  ####################
  # Do the NA substitution before preprocessing, if "before" is provided by the user
  if (!is.null(replace)) {
    if (replace[1] == "before") {
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
    for (i in 1:length(preprocess)) {
      if (attributes((preprocess[[i]]))$class == "dummyVars") {
        ModelForNames <- preprocess[[i]]
        # Retrive the original classes of the dataset's categorical vars
        for (i in 1:dim(df)[2]) {
          #Retrieve levels of factor
          if (attr(ModelForNames$terms, "dataClasses")[colnames(df)[i]] == "factor") {
            df[, i] <- factor(df[, i], levels = ModelForNames$lvls[colnames(df)[i]][[1]])
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
      for (i in 1:dim(df)[2]) {
        #Retrieve levels of factor
        if (attr(ModelForNames$terms, "dataClasses")[colnames(df)[i]] == "factor") {
          df[, i] <- factor(df[, i], levels = ModelForNames$xlevels[colnames(df)[i]][[1]])
        }
      }
    }

    #Data preprocessing
    for (i in 1:length(preprocess)) {
      preprocess.method <- preprocess[[i]]
      preprocessData <- predict(preprocess.method, df)
      df <- preprocessData
    }

  }


  # Do the NA substitution after preprocessing, if "after" is provided by the user
  if (length(replace) == 2) {
    if (replace[1] == "after") {
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
