# render ####

#' Generate a list of links to images
#' @param full directory of image files.
#' @param ncol number of columns, when fixed in the CSS flow or grid.
#' @param max maximum number of listed files.
#' @param pattern regular expression used to filter files list.
#' @param sort sort files by date or pattern.
#'   * now, older files last
#'   * past, older files first
#'   * none, listed order
#' @param group grouping variable for the lightbox viewer.
#'
make_gallery <- function(
    full = "img/gallery", ncol = 1, max = dplyr::n(), pattern = ".*",
    sort = "past", group = "default") {

  # list files
  file_full <- list.files(full, full.names = TRUE)

  sort_img <- switch(
    sort,
    "past" = function(d) dplyr::arrange(d, files),
    "now" = function(d) dplyr::arrange(d, dplyr::desc(files)),
    "none" = function(d) purrr::map_dfr(
      pattern, ~ dplyr::filter(d, stringr::str_detect(files, .x)))
  )

  # reorder files as a function of date or column structure.
  list_files <- dplyr::tibble(files = file_full) |>
    dplyr::filter(
      stringr::str_detect(files, paste(pattern, collapse = "|"))) |>
    sort_img() |>
    dplyr::slice(1:max) |>
    dplyr::mutate(
      row = (dplyr::row_number() - 1) %/% ncol,
      col = (dplyr::row_number() - 1) %% ncol) |>
    dplyr::arrange(col, row) |>
    dplyr::pull(files)

  # create links
  glue::glue(
    "<div> ![]({list_files}){{.lightbox group=\"{group}\"}} </div>")

}

#' Generate image figures captioned by date and series
#' @param files image file names, as yyyymmdd_series.ext
#' @param path directory of image files.
#' @param group grouping variable for the lightbox viewer.
#'
make_figures <- function(
    files, path = "posts/works/portfolio/img/gallery", group = "band") {

  caption <- files |>
    stringr::str_remove("\\.[a-z]+$") |>
    stringr::str_replace("^(\\d{4})(\\d{2})(\\d{2})_", "\\1-\\2-\\3, ")

  glue::glue(
    "![{caption}]({path}/{files})",
    "{{.lightbox group=\"{group}\" loading=\"lazy\"}}\n\n")

}
