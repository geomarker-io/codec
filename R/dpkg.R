#' Write a data package to a folder
#'
#' Write a [data package](https://datapackage.org/standard/data-package/) with
#' one [data resource](https://datapackage.org/standard/data-resource/) that
#' is any R object.
#' Within `dir`, the data package is created as a new folder based on the `name` of the object
#' and will contain a `{name}.rds` file and a `dataresource.yaml` file
#'
#' @param x object to save as a data package
#' @param name simple name or identifier for data package
#' @param version must be a `package_version()` object;
#' the [semantic version](https://datapackage.org/recipes/data-package-version/) of the data package
#' @param dir directory in which to write the data package
#' @param readme_file required; the name of a README.md file that
#' [describes](https://datapackage.org/standard/data-package/#description) the data package;
#' the `title` descriptor for the data package is taken from the first level one header of this file
#' @param source_file an optional path to a source file to copy to the data package and
#' reference in the `sources` descriptor
#' @returns invisibly, the path to the datapackage.yaml file;
#' also prints the directory tree of the created data package
#' @export
#' @examples
#' cat("# My cars", "\n", "This is all about the carsss.",
#'   file = fs::path(tempdir(), "README.md"), sep = "\n"
#' )
#' dpkg_write(mtcars, "mtcars", version = "0.1.0", dir = tempdir())
dpkg_write <- function(x,
                       name,
                       version,
                       dir = getwd(),
                       readme_file = fs::path(dir, "README.md"),
                       homepage = NULL,
                       source_file = NULL) {
  dpkg_folder <-
    fs::path(dir, name) |>
    fs::dir_create()
  readme_lines <- readLines(readme_file)
  title <-
    gsub(
      "^# ", "",
      readme_lines[grep("^# ", readme_lines)[1]]
    )
  dpkg_path <- fs::path(dpkg_folder, "datapackage.yaml")
  data_path <- fs::path(dpkg_folder, name, ext = "rds")
  saveRDS(x, data_path)
  descriptor <-
    list(
      `$schema` = "https://datapackage.org/profiles/2.0/datapackage.json",
      homepage = homepage,
      version = as.package_version(version),
      created = Sys.time(),
      resources =
        list(
          resource =
            list(
              `$schema` = "https://datapackage.org/profiles/2.0/dataresource.json",
              name = name,
              path = fs::path_rel(data_path, dpkg_folder),
              title = title,
              description = paste(readme_lines, collapse = "\n"),
              format = "rds",
              type = NULL,
              bytes = as.integer(file.size(data_path)),
              hash = digest::digest(x, algo = "md5")
            )
        )
    )
  if (!is.null(source_file)) {
    fs::file_copy(source_file, fs::path(dpkg_folder, "source", ext = "R"), overwrite = TRUE)
    descriptor$sources <- list(source = list(path = "source.R"))
  }
  yaml::as.yaml(descriptor) |>
    cat(file = dpkg_path)
  fs::dir_tree(fs::path(dir, name))
  return(invisible(dpkg_path))
}

#' Read a data package from a folder
#'
#' Read a [data package](https://datapackage.org/standard/data-package/) with
#' one [data resource](https://datapackage.org/standard/data-resource/) that
#' is a RDS object created with `dpkg_write()`.
#' @param dpkg path or url to folder containing a datapackage.yaml file
#' @param readRDS logical; read and return R object based on `path` descriptor?
#' @returns if readRDS is TRUE, then an R object saved read from RDS file;
#' otherwise, the path to the data resource
#' @export
#' @examples
#' dpkg_write(mtcars, "mtcars", version = "0.1.0", dir = tempdir())
#' dpkg_read(fs::path(tempdir(), "mtcars"))
dpkg_read <- function(dpkg, readRDS = TRUE) {
  md <-
    fs::path(dpkg, "datapackage.yaml") |>
    yaml::read_yaml()
  d_path <- fs::path(dpkg, md$resources$resource$path)
  if (readRDS) {
    return(readRDS(d_path))
  }
  return(d_path)
}

#' Put a dpkg into the CoDEC S3 bucket under `/data`
#'
#' Ensure {paws} can connect via the usual methods.
#' See the developer guide at https://www.paws-r-sdk.com/developer_guide/credentials/.
#' @param x path to datapackage.yaml file
#' @returns NULL
#' @examples
#' \dontrun{
#' # use credentials in .env file to authenticate with aws
#' library(dotenv)
#' # alternatively, use aws command line to login interactively via profile sso account
#' system2("aws", c("sso", "login", "--profile", "geomarker-io"))
#' # make sure to set AWS_PROFILE so {paws} can authenticate
#' Sys.setenv("AWS_PROFILE" = "geomarker-io")
#' }
dpkg_s3_put <- function(x) {
  dpkg_folder <- fs::path_dir(x)
  dpkg_name <- yaml::read_yaml(x)$resources$resource$name
  fls <- fs::dir_ls(dpkg_folder)
  for (f in fls) {
    paws::s3()$put_object(
      Body = f,
      Bucket = "io.geomarker.codec",
      Key = glue::glue("data/{dpkg_name}/", fs::path_file(f))
    )
  }
  return(NULL)
}

#' Get a dpkg file from the CoDEC S3 bucket under `/data`
#' @param x name of CoDEC dpkg
#' @param dir directory in which to write the data package
#' @returns path to downloaded datapackage directory
#' @examples
#' dpkg_s3_get("aadt")
dpkg_s3_get <- function(x = "aadt", dir = tools::R_user_dir("codec", "data")) {
  browser()
  dpkg_folder <- fs::path(dir, x)
  fs::dir_create(dpkg_folder)
  resp <-
    paws.storage::s3()$get_object(
      Bucket = "io.geomarker.codec",
      Key = glue::glue("data/{x}/datapackage.yaml")
    )
  writeBin(resp$Body, con = fs::path(dpkg_folder, "datapackage.yaml"))
  resp2 <-
    paws.storage::s3()$get_object(
      Bucket = "io.geomarker.codec",
      Key = glue::glue("data/{x}/{x}.rds")
    )
  writeBin(resp2$Body, con = fs::path(dpkg_folder, x, ext = "rds"))
  return(dpkg_folder)
  ## resp2$VersionId
}
