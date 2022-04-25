#!/usr/bin/env Rscript

suppressPackageStartupMessages(library("docopt"))

doc <- 
'Usage:
  render.R pdf <template> <input_dir>
  render.R list
  render.R (-h | --help)

Options:
  -h --help     Show help screen.'

args <- docopt(doc)

if(args$list) {
  # List the form templates available
  forms <- tools::file_path_sans_ext(list.files("forms", recursive = T, pattern = ".Rmd"))
  cat("--- FORMS ---", forms, sep = "\n")
} else if(args$pdf) {
  template <- args$template
  input_dir <- args$input_dir
  format <- "pdf_document"
  
  data_to_process <- list.files(input_dir, full.names = TRUE)
  for(jsondata in data_to_process) {
    cat("Converting", jsondata)
    rmarkdown::render(input = paste0("forms/", template, ".Rmd"),
                    output_dir = input_dir, # otherwise will be in form/template
                    output_format = format,
                    params = list(data = jsondata))
  }
} else {
  cat("Unknown option.")
}