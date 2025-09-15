# render ####

#' Generate a list of links to images
#' @param output format of the output table.
#' @param ncol number of columns, when fixed in the CSS flow or grid.
#' @param max maximum number of listed files.
#' @param sort sort files by date or pattern.
#'   * now, older files last
#'   * past, older files first
#'   * none, listed order
#' @param pattern regular expression used to filter files list. 
#' @param group grouping variable for the lightbox viewer. 
#'  
make_gallery <- function(
    scale = 0.24, output = "md",
    preview = "img/preview", full = "img/gallery",
    ncol = 1, max = dplyr::n(), pattern = ".*", sort = "past", 
    group = "default") {
  
  # list files
  file_preview <- list.files(preview, full.names = TRUE)
  file_full <- list.files(full, full.names = TRUE)
  
  switch(
    output,
    
    md = {
      
      sort_img <- switch(
        sort,
        "past" = function(d) dplyr::arrange(d, files),
        "now" = function(d) dplyr::arrange(d, dplyr::desc(files)),
        "none" = function(d) purrr::map_dfr(
          pattern, ~ dplyr::filter(d, stringr::str_detect(files, .x)))  
      )

      # reorder files as a function of date or column structure.
      list_files <- dplyr::tibble(files = file_full) |>
        dplyr::filter(stringr::str_detect(files, paste(pattern, collapse = "|"))) |> 
        sort_img() |>
        dplyr::slice(1:max) |> 
        dplyr::mutate(
          row = (dplyr::row_number() - 1) %/% ncol,
          col = (dplyr::row_number() - 1) %% ncol) |>
        dplyr::arrange(col, row) |>
        dplyr::pull(files)

      # create links
      links <- glue::glue("<div> ![]({list_files}){{.lightbox group=\"{group}\"}} </div>")
      
      return(links)
    },
    
    # generate a list of nested markdown links
    md_nested = {
      
      # set up column sizing
      scale <- glue::glue("{{width=\"{scale * 100}%\"}}")
      
      # create md links
      links <- glue::glue("[![]({file_preview}){scale}]({file_full})")
      
      return(links)
      
    },
    
    # generate a bootstrap container.
    # code adapted from https://github.com/djnavarro/hugo-diziet
    diziet = {
      
      # create images links
      links <- paste0(
        '<div class="g-col-6 g-col-md-3">',
        '<a href="', file_full, '">',
        '<img width = 100% src="', file_preview, '">',
        '</a>',
        '</div>'
      )
      
      # wrap in html and write to document
      cat('<div class="gal">')
      cat('<div class="container-fluid">')
      cat('<div class="grid">')
      cat(paste(links, collapse="\n"))
      cat('</div>\n')
      cat('</div>\n')
      cat('</div>\n')
      
    }
  )
  
}
