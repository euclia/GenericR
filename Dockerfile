# Use the official R base image with plumber installed
FROM rstudio/plumber:latest

# Install required R packages
RUN R -e "install.packages(c('RCurl', 'deSolve', 'rpart', 'party', 'tree', 'glmnet', 'Iso', 'naivebayes', 'neighbr', 'gbm', 'randomForest', 'e1071', 'truncnorm', 'xgboost', 'caret'), repos='http://cran.cc.uoc.gr/mirrors/CRAN/')"
RUN R -e "install.packages(c('neuralnet', 'bnlearn'), repos='http://cran.cc.uoc.gr/mirrors/CRAN/')"

# Copy the R scripts to the /app directory
COPY R /app/R

# Set the working directory
WORKDIR /app/R

# Expose the port plumber will run on
EXPOSE 8004

# Run the plumber API
CMD ["R", "-e", "pr <- plumber::plumb('predict.R'); pr$run(host='0.0.0.0', port=8004)"]
