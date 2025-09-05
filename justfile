# build pkgdown site
build_pkgdown_site:
    Rscript -e "devtools::build_site(quiet = FALSE, preview = FALSE)"

# make a CoDEC table by name
make codec_tbl_name:
    Rscript data-raw/codec_tbl/{{ codec_tbl_name }}/make.R

# make CoDEC latest annual data object
make_codec_latest_annual:
    Rscript data-raw/make_codec_latest_annual.R

# make CoDEC internal data
make_codec_internal_data:
    Rscript data-raw/make_internal_data.R
