AWS_PROFILE := env("AWS_PROFILE", "geomarker-io")
set export

# aws sso login
login_aws:
  aws sso login --profile {{AWS_PROFILE}}

# make and release CoDEC data
release_data codec_dpkg_name: login_aws
  Rscript inst/{{codec_dpkg_name}}/{{codec_dpkg_name}}.R

# print stuff
debug:
  echo $AWS_PROFILE
