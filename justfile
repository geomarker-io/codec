# make and release CoDEC data
release_data codec_dpkg_name:
  Rscript inst/codec_data/{{codec_dpkg_name}}/{{codec_dpkg_name}}.R
