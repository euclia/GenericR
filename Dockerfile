# Use the official R base image with plumber installed
FROM rstudio/plumber:latest

# Install required R packages
RUN R -e "install.packages(c('RCurl', 'deSolve', 'rpart', 'party', 'tree', 'glmnet', 'Iso', 'naivebayes', 'neighbr', 'gbm', 'randomForest', 'e1071', 'truncnorm', 'xgboost', 'caret', 'neuralnet', 'bnlearn'), repos='https://cloud.r-project.org/')"

# Expose the port plumber will run on
EXPOSE 8004

# Add app files from host's present working directory
COPY R /api

# Set working directory
WORKDIR /api

# Set default startup command to run the app's "plumber.R" file
CMD ["Rscript", "plumber.R"]
