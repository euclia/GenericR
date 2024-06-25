# Use the official R base image with plumber installed
FROM rocker/r-ver:4.1.0

LABEL maintainer="Pantelis Karatzas <pantelispanka@gmail.com>, Periklis Tsiros <ptsirostsib@gmail.com>"

# Install required R packages
RUN R -e "install.packages(c('plumber', 'jsonlite', 'deSolve', 'RCurl', 'rpart', 'party', 'tree', 'glmnet', 'Iso', 'naivebayes', 'neighbr', 'gbm', 'randomForest', 'e1071', 'truncnorm', 'xgboost', 'caret', 'neuralnet', 'bnlearn'), repos='http://cran.us.r-project.org')"

# Copy the R scripts to the /app directory
COPY R /app/R

# Set the working directory
WORKDIR /app/R

# Expose the port plumber will run on
EXPOSE 8004

# Run the plumber API
CMD ["R", "-e", "pr <- plumber::plumb('predict.R'); pr$run(host='0.0.0.0', port=8004)"]
