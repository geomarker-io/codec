# build pkgdown site
build_pkgdown_site:
    Rscript -e "devtools::build_site(quiet = FALSE, preview = FALSE)"

# make CoDEC tables
make codec_tbl_name:
    Rscript data-raw/codec_tbl/{{ codec_tbl_name }}/make.R

# make CoDEC latest annual
make_codec_latest_annual:
    Rscript data-raw/make_codec_latest_annual.R
