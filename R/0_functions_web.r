# render ####

#' Generate a list of links to images
#' @param output format of the output table.
#' @param ncol number of columns, when fixed in the CSS flow or grid.
#' @param max maximum number of listed files.
#' @param reverse sort files by name (date), default to older first.
#' @param pattern regular expression used to filter files list. 
#' @param group grouping variable for the lightbox viewer. 
#'  
make_gallery <- function(
    scale = 0.24, output = "md",
    preview = "img/preview", full = "img/gallery",
    ncol = 1, max = dplyr::n(), reverse = FALSE, pattern = NULL, group = "default") {
  
  # list files
  file_preview <- list.files(preview, full.names = TRUE, pattern = pattern)
  file_full <- list.files(full, full.names = TRUE, pattern = pattern)
  
  switch(
    output,
    
    md = {
      
      # reorder files as a function of date or column structure.
      list_files <- dplyr::tibble(file = file_full) |>
        dplyr::arrange(if (reverse) dplyr::desc(file) else file) |>
        dplyr::slice(1:max) |> 
        dplyr::mutate(
          row = (dplyr::row_number() - 1) %/% ncol,
          col = (dplyr::row_number() - 1) %% ncol) |>
        dplyr::arrange(col, row) |>
        dplyr::pull(file)

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
