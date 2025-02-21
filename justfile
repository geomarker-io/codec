# build pkgdown site
build_pkgdown_site:
  Rscript -e "devtools::build_site()"

# make and release CoDEC data
release_data codec_dpkg_name:
  cd inst/codec_data/{{codec_dpkg_name}} &&
  Rscript {{codec_dpkg_name}}.R

# build and view shiny application
build_shiny:
  Rscript -e "shiny::runApp('./inst/codec_shiny', launch.browser = TRUE)"

# build and view shiny application
build_shiny_catalog:
  Rscript -e "shiny::runApp('./inst/codec_catalog', launch.browser = TRUE)"

# build and view shinylive data catalog
build_catalog:
  Rscript -e "devtools::load_all()" \
  -e "source('inst/codec_catalog/make_all_codec_dpkg.R')" && \
  Rscript -e "shinylive::export('inst/codec_catalog', 'inst/codec_catalog_site')" \
    -e "httpuv::runStaticServer('inst/codec_catalog_site')"
  # Rscript -e "shiny::runApp('./inst/codec_catalog', launch.browser = TRUE)"

