predict.pbpk <- function(dataset, rawModel, additionalInfo){
  
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
  
  mod <- unserialize(jsonlite::base64_dec(rawModel))
  covmodel <- mod$COVMODEL
  odemodel <- mod$ODEMODEL

  comp  <- additionalInfo$fromUser$comp
  initial_concentration <- rep(0,length(comp)) 
  for(i in 1:length(comp)){
    con <- paste("C0_", comp[i], sep="")
    initial_concentration[i] = df[[con]]
  }
  
  cov.pars  <- sapply(additionalInfo$fromUser$cov.pars, function(x) df[[x]])
  t_inf <- df$infusion_time
  dose <- df$dose
  
  params<-c(covmodel(cov.pars), dose, t_inf)
  sample_time <- seq(df$time.start , df$time.end, df$time.by)  # in hours
  solution <- ode(y = initial_concentration, times = sample_time, func = odemodel, parms = params)

  for(i in 1:dim(solution)[1]){
    prediction<- data.frame(t(solution[i,]))
    colnames(prediction)<- c("time", comp)
    if(i==1){lh_preds<- list(jsonlite::unbox(prediction))
    }else{
      lh_preds[[i]]<- jsonlite::unbox(prediction)
    }
  }
  datpred <-list(predictions=lh_preds)
  
  return(datpred)
}
