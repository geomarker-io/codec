#' Use a dpkg to create a github release
#'
#' The release will be tagged at the current commit and named
#' according to the `name` and `version` of the dpkg.
#' Any `description` metadata (auto populated at time of dpkg creation using a README.md file)
#' will also be used as the GitHub release description.
#' The `GITHUB_PAT` environment variable must be set and the working directory
#' must be inside of a git repository with a GitHub remote.
#' @param x path to datapackage.yaml
#' @param draft logical; mark release as draft?
#' @return the URL to the release (invisibly)
#' @export
dpkg_gh_release <- function(x, draft = TRUE) {
  rlang::check_installed("gert", "get current git commit")
  rlang::check_installed("gh", "create a release on github")
  rlang::check_installed("httr2", "add assets to a github release")
  dpkg_folder <- fs::path_dir(x)
  dpkg_name <- yaml::read_yaml(x)$resources$resource$name
  dpkg_version <- paste(yaml::read_yaml(x)$version[[1]], collapse = ".")

  gh_owner <- gh::gh_tree_remote()$username
  gh_repo <- gh::gh_tree_remote()$repo

  draft_release_details <-
    gh::gh(
      glue::glue("POST /repos/{gh_owner}/{gh_repo}/releases"),
      name = glue::glue("{dpkg_name} {dpkg_version}"),
      tag_name = glue::glue("{dpkg_name}-v{dpkg_version}"),
      target_commitish = gert::git_info()$commit,
      body = yaml::read_yaml(x)$resources$resource$description,
      draft = draft
    )
  gh_release_id <- draft_release_details$id

  put_asset_req <-
    glue::glue("https://uploads.github.com/repos/{gh_owner}/{gh_repo}/releases/{gh_release_id}/assets") |>
    httr2::request() |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Accept = "application/vnd.github+json",
      Authorization = glue::glue("Bearer {Sys.getenv('GITHUB_PAT')}"),
      `Content-Type` = "application/octet-stream",
      `X-GitHub-Api-Version` = "2022-11-28",
      .redact = "Authorization"
    )

  put_asset_req |>
    httr2::req_url_query(name = "datapackage.yaml") |>
    httr2::req_body_file(x) |>
    httr2::req_perform()

  put_asset_req |>
    httr2::req_url_query(name = glue::glue("{dpkg_name}.rds")) |>
    httr2::req_body_file(fs::path(dpkg_folder, dpkg_name, ext = 'rds')) |>
    httr2::req_perform()

  message("created draft release at: ", draft_release_details$html_url)
  return(invisible(draft_release_details$html_url))

}
