setwd("/Users/edwardarubin/Dropbox/Teaching/ARE212")

my_render <- function(site) {
  # Build the site
  rmarkdown::render_site(site)
  # Clean up all objects
  rm(list = ls())
  # Unload packages
  library(pacman)
  p_unload(all)
}

# Render the section pages
sites <- stringr::str_pad(1:13, 2, "left", 0)
sites <- c(sites, "12b")
for (i in sites) my_render(paste0("section", i, ".Rmd"))

# Render other pages
more <- paste0(c("contact", "courseInfo", "index", "latexKnitr",
  "notes", "resources", "syllabi"), ".Rmd")
lapply(X = more, FUN = my_render)
