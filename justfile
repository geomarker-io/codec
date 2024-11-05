# make and release CoDEC data
release_data codec_dpkg_name:
  cd inst/codec_data/{{codec_dpkg_name}} &&
  Rscript {{codec_dpkg_name}}.R

# build and view shiny application
build_shiny:
  Rscript -e "shiny::runApp('./inst/codec_shiny', launch.browser = TRUE)"

