predict.pbpk <- function(dataset, rawModel, additionalInfo){
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
  df <- data.frame(matrix(0, ncol = 0, nrow = rows_data))
  
  for(key in feat.keys){
    # For each key (feature) get the vector of values (of length 'row_data')
    feval <- dataset$dataEntry$values[key][,1]
    # Name the column with the corresponding name that is connected with the key
    df[key.match[key.match$feat.keys == key, 2]] <- feval
  }
 
  # Unserialize the ODEs and the covariate model
  mod <- unserialize(jsonlite::base64_dec(rawModel))
  covmodel <- mod$COVMODEL
  odemodel <- mod$ODEMODEL
  # Get the names of compartments in the same order as represented by the ODEs
  comp  <- additionalInfo$fromUser$comp
  # Get the input compartment
  incomp <- additionalInfo$fromUser$incomp  
  # Calculate the initial concentrations
  initial_concentration <- rep(0,length(comp)) 
  for(i in 1:length(comp)){
    # Create a string for each compartment e.g. init_MU
    con <- paste("init_", comp[i], sep="")
    # Read the corresponding initial concentration from the user dataset
    initial_concentration[i] = df[[con]]
    # If the dose is non zero but t_inf is zero then we have to insert 
    # instantaneously the whole dose to the initial concentration of the 
    # entry compartment
    if ((comp[i] == incomp) && (dose != 0) && (t_inf == 0)){
       initial_concentration[i] = dose
    }
  }
  # Get infusion time and dose        
  t_inf <- df$infusion_time
  dose <- df$dose
  # If a covariate model exists
  if (!is.null(covmodel)){
    # Get the values of the covariates
    cov.pars  <- sapply(additionalInfo$fromUser$cov, function(x) df[[x]])
    # Create a parameter vector including the generated physiological parameters
    params<-c(covmodel(cov.pars), dose, t_inf)
  } else {
    # If no covariate model => only parameters are dose and infusion time
    params<-c(dose, t_inf)
  }

  # Generate a time vector based on the user input
  sample_time <- seq(df$time.start , df$time.end, df$time.by) 
  # Integrate the ODEs using the deSolve package
  solution <- deSolve::ode(y = initial_concentration, times = sample_time, func = odemodel, parms = params)

  for(i in 1:dim(solution)[1]){
    prediction<- data.frame(t(solution[i,]))
    # Name the predictions
    colnames(prediction)<- c("time", comp)
    # Bring everything into a format that cooperates with Jaqpot
    if(i==1){lh_preds<- list(jsonlite::unbox(prediction))
    }else{
      lh_preds[[i]]<- jsonlite::unbox(prediction)
    }
  }
  datpred <-list(predictions=lh_preds)
  
  return(datpred)
}
