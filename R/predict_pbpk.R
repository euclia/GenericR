predict.pbpk <- function(dataset, rawModel, additionalInfo){

  ###########################################
  ### Create input vector from Jaqpot format
  ##########################################

  # Get the number of compartments of the PBPK model
  n_comp <- length(additionalInfo$predictedFeatures) - 1
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
  data.feats <- list()

  for(key in feat.keys){
    # For each key (feature) get the vector of values (of length 'row_data')
    feval <- dataset$dataEntry$values[key][,1]
    # Name the column with the corresponding name that is connected with the key
    data.feats[key.match[key.match$feat.keys == key, 2]] <- feval
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


  # Unserialize the ODEs and the covariate model
  mod <- unserialize(jsonlite::base64_dec(rawModel))
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
  params <- create.params(data.feats)
  # Create initial conditions
  inits <- create.inits(params)
  # Create events
  events <- create.events(params)


  # Get the names of compartments in the same order as represented by the ODEs
  ###comp  <- additionalInfo$fromUser$comp


  # Generate a time vector based on the user input
  sample_time <- seq(data.feats$sim.start , data.feats$sim.end, data.feats$sim.step)

  # Integrate the ODEs using the deSolve package
  solution <- do.call(deSolve::ode, c(list(times = sample_time,  func = ode.func, y = inits, parms = params,
                                           custom.func = custom.func, method = ode.method,  events = events), extra.args))
  # Keep only the output dictated by the model uploader through predicted.feats
  predicted.feats <- rep(0,  length(additionalInfo$predictedFeatures))
  for (i in 1:length(predicted.feats)){
    predicted.feats[i] <- additionalInfo$predictedFeatures[[i]]
  }
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
