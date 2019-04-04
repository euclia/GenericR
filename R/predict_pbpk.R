

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
  
  mod <- unserialize(base64_dec(rawModel))
  covmodel <- mod$COVMODEL
  odemodel <- mod$ODEMODEL
  
  # predFeat <- additionalInfo$predictedFeatures[1][[1]]
  predFeat <- additionalInfo$predictedFeatures
  
  comp  <- additionalInfo$comp
  initial_concentration <- rep(0,length(comp)) 
  for(i in length(comp)){
    con <- paste("C0_", comp[i], sep="")
    initial_concentration[i] = df[[con]]
  }
  
  cov.pars <- df[,1:(dim(df)[2]-5-length(comp))]  
  t_inf <- df$infusion_time
  dose <- df$dose
  
  params<-c(covmodel(cov.pars), t_inf, dose)
  sample_time <- seq(df$time.start , df$time.end, df$time.by)  # in hours
  solution <- ode(y = initial_concentration, times = sample_time, func = odemodel, parms = params)
  
  #comp_names <- rep(0, n_comp + 1)
  #comp_short <- rep(0, n_comp + 1)
  #pred_names <- rep(0, n_comp + 1)
  #for(i in 1:length(comp_names)){
  #  comp_names[i] <- additionalInfo$predictedFeatures[[i]]
  #  comp_short[i] <- as.integer(strsplit(additionalInfo$predictedFeatures[[i]], "_")[[1]])
  #  pred_names[i] <- names(additionalInfo$predictedFeatures)[i]
  #}
  #comp_all <- data.frame(cbind(comp_names, comp_short, pred_names))
  #comp_all_s <- comp_all[order(comp_short),][,1]
  
  
  
  for(i in 1:dim(solution)[1]){
    prediction<- data.frame(t(solution[i,]))
    colnames(prediction)<- c("time", comp)
    if(i==1){lh_preds<- list(unbox(prediction))
    }else{
      lh_preds[[i]]<- unbox(prediction)
    }
  }
  datpred <-list(predictions=lh_preds)
  
  return(datpred)
}