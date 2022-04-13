library(htmltools)
library(magrittr)

#' Read YAML config file for inputs
#' 
#' TODO: Validate config with read
#' @param yaml YAML configuration file.
getConfig <- function(yaml = "dsp-inputs.yml") {
  yaml::read_yaml(yaml)
}

#' RIA: Render Input Appropriately
#' 
#' @param x Either an input specification or data to render
RIA <- function(x, mode = knitr::opts_knit$get('rmarkdown.pandoc.to')) {
  if(mode == "latex") {
    pdfProcess(x)
  } else if(mode == "html") {
    newInput(x) 
  } else {
    message("Input not processed:", x)
  }
} 

pdfProcess <- function(x) {
  if(is.null(x)) return("\\detokenize{_____}")
  if(class(x) %in% "data.frame") return(knitr::kable(x))
  if(length(x) == 1 && x %in% c("TRUE", "on")) return("\\detokenize{__X__}")
  # otherwise, response is some text
  if(length(x) == 1 && x == "") return("\\detokenize{_____}")
  if(length(x) > 1 && "hidden" %in% x) return("")
  if(length(x) > 1) return(paste(x, collapse = ", "))
  words <- strsplit(x, " ")[[1]]
  if(length(words) > 10) {
    sprintf("\\parbox{6in}{\\detokenize{%s}}", x)
  } else {
    sprintf("\\detokenize{%s}", x)
  }
}

#' Create HTML form input
#' 
#' Handles common types `c("select", "text", "textarea", "date", "number", "email")`
#' as well as specially defined types called `table-inputs` and `select-schema`.
#' 
#' @param input List containing input config.
newInput <- function(input) {
  type <- input$type
  if(!length(type)) stop("Input has not been defined in configuration!")
  switch(type,
         "table-inputs" = tableInputWrapper(input),
         "select-schema" = schemaSelectInput(input),
         "select" = customSelectInput(input),
         "textarea" = div(tags$textarea(name = input$name, 
                                        form = input$form, 
                                        class = input$class, 
                                        placeholder = input$placeholder)),
         do.call(tags$input, input)
         )
}

#' Batch dispatch to create HTML form inputs
newInputs <- function(inputs) {
  lapply(inputs, newInput)
}

#' Create form elements with table layout
#'
#' Sometimes it is highly preferable to organize repeated elements in a table,
#' similar to a banking form where one might have multiple rows each with columns:
#' bank account name, bank account type, bank account number.
#' 
#' @param tabinput List contain specifications for inputs. 
#' @param nrow Number of rows, i.e. how many times to repeat.
#' @param removable Whether rows can be deleted, by default TRUE.
tableWithInput <- function(tabinput, nrow = 3, removable = TRUE) {
  inputs <- tabinput$inputs
  id <- tabinput$id
  rows <- tagList()
  for(i in 1:nrow) {
    rows[[i]] <- newInputs(inputs) %>% 
      newCell() %>% 
      newRow(removable = removable)
  }
  rows <- rows %>% tags$tbody()
  hcells <- lapply(inputs, function(input) newCell(input$display, header = TRUE))
  header <- hcells %>% newRow(removable = F) %>% tags$thead()
  table <- tags$table(id = id) %>% tagAppendChildren(header, rows)
  table
}

#' Create button to add a row
addRowBtn <- function(tabinput) {
  id <- tabinput$id
  stopifnot(!is.null(id))
  tags$button(tags$i(class = "glyphicon glyphicon-plus"), "entry",
              class = "add-button",
              onClick = sprintf("addRow('%s')", id)
  )
}

#' Wrapper to render table input
#' 
#' @inheritParams tableWithInput
tableInputWrapper <- function(tabinput) {
  add <- tabinput$add # TRUE creates a button allowing new rows to be added to table
  tb <- tableWithInput(tabinput)
  btn <- if(add) addRowBtn(tabinput) else NULL
  htmltools::div(class = "table-input-container",
    htmltools::div(tb), btn
  )
}

#' Wrap content in a table cell
newCell <- function(content, header = FALSE) {
  cell <- lapply(content, function(x) if(header) tags$th(x) else tags$td(x))
  cell
}


#' Wrap cells in table row
newRow <- function(cells, removable = TRUE) {
  row <- tags$tr() %>% tagAppendChildren(cells) 
  if(removable) {
    # optional element at row end to remove the row
    x <- tags$td(
      tags$i(class = "glyphicon glyphicon-minus-sign",
             onclick = "removeRow(this);")
            )
    row <- row %>% tagAppendChild(x)
  }
  row
}

#' Select input with controlled values from schema
#' 
#' Create a select input where options are schema-based, 
#' i.e. defined by a specified class such as "Assay"
#' @inheritParams newInput
schemaSelectInput <- function(input) {
  
  options <- nfportalutils::get_valid_values_from_json_schema(
    schema_url = input$schema, 
    parent_name = input$range
    )
  customSelectInput(input, options)
}

#' @inheritParams newInput
#' @param options Vector of select options. If not given, will be extracted from input specs.
customSelectInput <- function(input, options = NULL) {
  select <- do.call(tags$select, input)
  if(!is.null(input$multiple) && input$multiple == TRUE) select %>% tagAppendAttributes("multiple" = NA)
  if(is.null(options)) options <- input$options # TO DO: Warn if no options from either
  if(!is.null(input$sort) && input$sort == TRUE) options <- sort(options) # alphabetize
  if(!is.null(input$blankstr) && input$blankstr == TRUE) options <- c("", options) # add "" default value
  options <- lapply(options, function(opt) tags$option(value = opt, opt))
  select %>% tagAppendChildren(options)
}

#' Create a checklist
#' 
#' Delegates to appropriate functions to create a checklist element
#' according to given configuration.
#' @param config List with checklist configuration.
newChecklist <- function(config) {
  items <- sapply(config$items, function(x) x$name)
  check_items <- lapply(items, makeCheckItem)
  divChecklist(check_items)
}

#' Create checklist parent element
divChecklist <- function(...) {
  div(id = "checklist", class = "note-card noprint", h4("My checklist")) %>% 
    tagAppendChildren(...)
}

#' Create checklist item
makeCheckItem <- function(name = "Item 1", class = "check-item") {
  id <- make.names(name)
  div(
    tags$input(type = "checkbox", name = name, class = class),
    tags$label(`for` = id, name)
  )
}

#' Element to submit form
#' 
#' Currently, this is just a shorthand that calls`formSubmitButton` because
#' the only configuration allowed is a button.
formSubmit <- function(config_submit) {
  
  formSubmitButton(id = config_submit$form, 
                   action = config_submit$action)
}

#' Create button to submit form
#' 
#' @param id Id of the form.
#' @param action Form action URL (defaults to a test URL). 
#' @param label Button label.
formSubmitButton <- function(id, 
                             action = "https://submit-form.com/echo",
                             label = "Submit"
                             ) {
  tags$form(id = id,
            method = "post",
            action = action,
            tags$button(type = "submit", label, class = "submit-button")
  )
}

