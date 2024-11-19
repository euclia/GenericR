predict.pbpk <- function(modelDto, datasetDto){

  #################################
  ## Input retrieval from Jaqpot ##
  #################################
  additionalInfo<-  modelDto$legacyAdditionalInfo
  rawModel<- modelDto$rawModel


  # Get feature names (actual name)
  feat.names <- modelDto$independentFeatures$name
  # Get feature types among FLOAT, INTEGER, STRING, TEXT, SMILES
  feat.types <-  modelDto$independentFeatures$featureType
  names(feat.types) <- feat.names
  # Get input values
    # Get input values
  df_init <- datasetDto$input
  df <- list()

  # Convert data types
  for (j in 1:dim(df_init)[2]){
    if(colnames(df_init)[j] == "jaqpotRowId"){
      df[[j]] = "jaqpotRowId"
    }else if (feat.types[colnames(df_init)[j]] == "FLOAT"){
      df[[j]] <- as.numeric( df_init[,j] )
    }else if (feat.types[colnames(df_init)[j]] == "INTEGER"){
      df[[j]] <- as.integer( df_init[,j] )
    }else if (feat.types[colnames(df_init)[j]] == "FLOAT_ARRAY"){
      df[[j]] <- as.numeric(df_init[,j][[1]])
    }else{
      # We don't need to do any conversion from STRING/ CATEGORICAL/ TEXT
      # as the input is already in a string format
        df[[j]] <- df_init[,j]
    }
  }
  names(df) <- colnames(df_init)

  # Check if the model includes any ellipses arguments (...), which the model creator
  # uses to define parameters of the solver
  ode.method <- additionalInfo$fromUser$method
  if (!(ode.method %in% c("lsoda", "lsode", "lsodes", "lsodar", "vode", "daspk","euler", "rk4", "ode23",
                          "ode45", "radau","bdf", "bdf_d", "adams", "impAdams", "impAdams_d", "iteration"))){
    ode.method <- "lsodes"
  }
  extra.args <- additionalInfo$fromUser$extra.args

  ####################################
  ### Continue with prediction process
  ####################################

  # Obtain decoded base 64 object
  decoded <- jsonlite::base64_dec(rawModel)
  # Unserialize the ODEs and the covariate model
  mod <- unserialize(decoded)
  # Extract function for parameter creation
  create.params <- mod$create.params
  # Extract function for initial conditions of odes creation
  create.inits <- mod$create.inits
  # Extract function for event creation
  create.events <- mod$create.events
  # Extract custom function
  custom.func <- mod$custom.func
  # Extract odes function
  ode.func <- mod$ode.func

  # Create parameter vector
  params <- create.params(df)
  # Create initial conditions
  inits <- create.inits(params)
  # Create events
  events <- create.events(params)


  # Get the names of compartments in the same order as represented by the ODEs
  ###comp  <- additionalInfo$fromUser$comp


  # Generate a time vector based on the user input
  sample_time <- seq(df$sim.start , df$sim.end, df$sim.step)

  # Integrate the ODEs using the deSolve package
  solution <- do.call(deSolve::ode, c(list(times = sample_time,  func = ode.func, y = inits, parms = params,
                                           custom.func = custom.func, method = ode.method,  events = events), extra.args))
  # Convert output from float to scientific notation
  solution <- formatC(solution, format = "e", digits = 3)
  #The following chunk of code was deleted because  it produces misleading plots
  #Select only the rows that correspond to the simulation time vector provided by the user
  #solution <- solution[solution[,1] %in% sample_time,]

  # Keep only the output dictated by the model uploader through predicted.feats
  predicted.feats <- rep(0,  length(modelDto$dependentFeatures))
  for (i in 1:length(predicted.feats)){
    predicted.feats[i] <- modelDto$dependentFeatures[[i]]['key']
  }
  ## IMPORTANT!!! Here if predicted.feats don't match with the solution names an error is flagged. A code resolving this
  # issue should be inserted in the future and clarify this in the manual for model uploaders!!!!!!
  solution_tr <- solution[,predicted.feats]

  for(i in 1:dim(solution_tr)[1]){
    ###### The following is a clumsy solution to the following problem:jsonlite doesnt know how to convert nan values
    for(j in 1:dim(solution_tr)[2]){
      if(is.nan(solution_tr[i,j])){
        solution_tr[i,j] <- 0
      }
    }
    prediction<- data.frame(t(solution_tr[i,]))
    # Bring everything into a format that cooperates with Jaqpot
    if(i==1){lh_preds<- list(jsonlite::unbox(prediction))
    }else{
      lh_preds[[i]]<- jsonlite::unbox(prediction)
    }
  }
  datpred <-list(predictions=lh_preds)


  return(datpred)
}
