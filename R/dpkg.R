#' Read a data package from a folder
#'
#' Read a [data package](https://datapackage.org/standard/data-package/) with
#' one [data resource](https://datapackage.org/standard/data-resource/) that
#' is a RDS object created with `dpkg_write()`.
#' @param dpkg path or url to folder containing a datapackage.yaml file
#' @param return what to return? "rds" returns the R object by reading in the rds file;
#' "path" returns the path to the rds file; "metadata" (or "md") returns a list of the metadata
#' stored with the rds file in the data package with `dpkg_write()`
#' @param md 
#' @param readRDS logical; read and return R object based on `path` descriptor?
#' @returns if readRDS is TRUE, then an R object saved read from RDS file;
#' otherwise, the path to the data resource
#' @export
#' @examples
#' dpkg_write(mtcars, "mtcars", version = "0.1.0", dir = tempdir())
#' dpkg_read(fs::path(tempdir(), "mtcars"))
dpkg_read <- function(dpkg, return = c("rds", "path", "metadata", "md")) {
  return_choice <- rlang::arg_match(return)
  md <-
    fs::path(dpkg, "datapackage.yaml") |>
    yaml::read_yaml()
  if (return_choice %in% c("metadata", "md")) {
    return(md)
  }
  d_path <- fs::path(dpkg, md$resources$resource$path)
  if (return_choice == "path") {
    return(d_path)
  }
  if (return_choice == "rds") {
    return(readRDS(d_path))
  }
}

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
