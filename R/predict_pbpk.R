predict.pbpk <- function(modelDto, datasetDto, additionalInfo, rawModel, doaDto){

  #################################
  ## Input retrieval from Jaqpot ##
  #################################

  # Get feature names (actual name)
  feat.names <- modelDto$independentFeatures$name
  if (class(datasetDto$input$values[[1]]) %in% c("matrix", "array")) {
    # Initialize a dataframe with as many rows as the number of values per feature
    rows <- dim(datasetDto$input$values[[1]])[1]
    cols <- dim(datasetDto$input$values[[1]])[2]
    df <- data.frame(matrix(NA, ncol = cols, nrow = rows))
    colnames(df) <- feat.names

    for (row in 1:rows) {
      for (column in 1:cols) {
        df[row, column] <- datasetDto$input$values[[1]][row, column]
      }
    }

  } else {
    df <- data.frame(matrix(datasetDto$input$values[[1]], ncol = length(feat.names), nrow = 1))
    colnames(df) <- feat.names
  }

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

  print(df$BW)
  print(is.character(df$BW))
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
  predicted.feats <- rep(0,  length(additionalInfo$predictedFeatures))
  for (i in 1:length(predicted.feats)){
    predicted.feats[i] <- additionalInfo$predictedFeatures[[i]]
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
