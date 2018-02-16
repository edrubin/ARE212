library(rmarkdown)
setwd("/Users/edwardarubin/Dropbox/Teaching/ARE212")
render("section05.Rmd",
  pdf_document(
    latex_engine = "xelatex",
    toc = T,
    number_sections = T,
    highlight = "pygments",
    pandoc_args = c(
      "--metadata=author:\"Ed Rubin\"",
      pandoc_variable_arg("mainfont", "Charter"),
      pandoc_variable_arg("monofont", "Hack"),
      pandoc_variable_arg("fontsize", "11pt")
      )
    ),
  output_dir = "Section05")
