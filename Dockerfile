FROM opencpu/base

LABEL Pantelis Karatzas <pantelispanka@gmail.com>
RUN apt-get -y install libxml2-dev libcurl4-openssl-dev

RUN R -e "install.packages(c('RCurl', 'jsonlite'), repos='http://cran.cc.uoc.gr/mirrors/CRAN/')"
COPY GenericR_0.1.0.tar.gz /packages/
USER root
RUN R CMD INSTALL /packages/GenericR_0.1.0.tar.gz --library=/usr/local/lib/R/site-library

CMD /usr/sbin/apache2ctl -D FOREGROUND
