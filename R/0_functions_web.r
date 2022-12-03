# render ####

# generate a list of markdown links to images
make_gallery <- function(
    scale = 0.24, output = "html",
    preview = "img/preview", full = "img/gallery") {
  
  # list files
  file_preview <- list.files(preview, full.names = TRUE)
  file_full <- list.files(full, full.names = TRUE)
  
  switch(
    output,
    
    # generate a list of markdown links
    md = {
      
      # set up column sizing
      scale = glue::glue("{{width=\"{scale * 100}%\"}}")
      
      # create md links
      preview <- glue::glue("![]({file_preview})")
      full <- glue::glue("![]({file_full})")
      
      links <- glue::glue("[![]({file_preview}){scale}]({file_full})")
      
      return(links)
      
    },
    
    # generate a bootstrap container.
    # code adapted from https://github.com/djnavarro/hugo-diziet
    html = {
      
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
