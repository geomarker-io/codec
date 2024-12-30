# make and release CoDEC data
release_data codec_dpkg_name:
  cd inst/codec_data/{{codec_dpkg_name}} &&
  Rscript {{codec_dpkg_name}}.R

# build and view shiny application
build_shiny:
  Rscript -e "shiny::runApp('./inst/codec_shiny', launch.browser = TRUE)"

# build and view shinylive data catalog
build_catalog:
  Rscript -e "shinylive::export('inst/codec_shiny', 'inst/codec_catalog_site')" \
    -e "httpuv::runStaticServer('inst/codec_catalog_site')"
  # Rscript -e "shiny::runApp('./inst/codec_catalog', launch.browser = TRUE)"

