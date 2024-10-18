# to loosely document dependencies and versions
# scan working directory for R code package dependencies
# get installed versions of package from library
# document in DESCRIPTION file

dsc <- desc::desc(text = "")
dsc$set(
  type = "package",
  Title = "CoDEC Shiny",
  R.version = paste(getRversion(), sep = ".")
)

deps <-
  tibble::tibble(
    type = "Imports",
    package = renv::dependencies(path = "inst/codec_shiny", quiet = TRUE)$Package,
    version = lapply(package, \(.) utils::packageDescription(.)$Version)
)
deps$version <- paste0(">=", as.character(deps$version))
dsc$set_deps(deps)

dsc

dsc$write(file = "inst/codec_shiny/DESCRIPTION")

# to install project, run:
# TODO this still doesn't work when in a subfolder of a package
pak::local_install_dev_deps(root = "inst/codec_shiny", upgrade = FALSE)

# to fully snapshot packages, point to DESCRIPTION file and specify explicit
renv::snapshot(project = "inst/codec_shiny", type = "explicit")
