FROM rocker/shiny:4.4.3

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    cmake \
    gdal-bin \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libgdal-dev \
    libgeos-dev \
    libharfbuzz-dev \
    libjpeg-dev \
    libpng-dev \
    libproj-dev \
    libssl-dev \
    libtiff-dev \
    libudunits2-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /srv/codec

RUN install2.r --error --skipinstalled remotes shiny bslib leaflet plotly htmltools

COPY DESCRIPTION /tmp/codec/DESCRIPTION
RUN R -q -e "remotes::install_deps('/tmp/codec', dependencies = c('Depends', 'Imports', 'LinkingTo'), upgrade = 'never')"

COPY . /srv/codec
RUN R CMD INSTALL .

EXPOSE 8080

CMD ["R", "-q", "-e", "port <- suppressWarnings(as.integer(Sys.getenv('PORT', '8080'))); if (is.na(port)) port <- 8080L; shiny::runApp('/srv/codec/inst/codec_catalog', host = '0.0.0.0', port = port)"]
