FROM opencpu/ubuntu-20.04

LABEL Pantelis Karatzas <pantelispanka@gmail.com>
LABEL Periklis Tsiros <ptsirostsib@gmail.com>
#RUN apt-get -y install libxml2-dev libcurl4-openssl-dev

RUN R -e "install.packages(c('RCurl', 'deSolve', 'rpart', 'party', 'tree', 'glmnet', 'Iso', 'naivebayes', 'neighbr', 'gbm', 'randomForest', 'e1071', 'truncnorm', 'xgboost', 'caret'), repos='http://cran.cc.uoc.gr/mirrors/CRAN/')"
RUN R -e "install.packages(c('neuralnet', 'bnlearn'), repos='http://cran.cc.uoc.gr/mirrors/CRAN/')"
COPY GenericR_1.1.8.1.tar.gz /packages/
USER root

RUN R CMD INSTALL /packages/GenericR_1.1.8.1.tar.gz --library=/usr/local/lib/R/site-library

CMD /usr/sbin/apache2ctl -D FOREGROUND
