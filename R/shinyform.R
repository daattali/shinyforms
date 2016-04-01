library(shiny)

#' @export
STORAGE_TYPES <- list(
  FLATFILE = "flatfile",
  SQLITE = "sqlite",
  MYSQL = "mysql",
  MONGO = "mongo",
  GOOGLE_SHEETS = "gsheets",
  DROPBOX = "dropbox",
  AMAZON_S3 = "s3"
)

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <- "
.mandatory_star { color: red; }
.shiny-input-container { margin-top: 25px; }
.sf_submit_msg { margin-left: 15px; }
.sf_error { color: red; }
"

saveData <- function(data, storage) {
  if (storage$type == STORAGE_TYPES$FLATFILE) {
    saveDataFlatfile(data, storage)
  } else if (storage$type == STORAGE_TYPES$GOOGLE_SHEETS) {
    saveDataGsheets(data, storage)
  }
}

saveDataFlatfile <- function(data, storage) {
  fileName <- paste0(
    paste(
      format(Sys.time(), "%Y%m%d-%H%M%OS"),
      digest::digest(data, algo = "md5"),
      sep = "_"
    ),
    ".csv"
  )
  
  resultsDir <- storage$path
  
  # write out the results
  write.csv(x = data, file = file.path(resultsDir, fileName),
            row.names = FALSE, quote = TRUE)
}
loadDataFlatfile <- function() {
  files <- list.files(file.path(resultsDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  data <- do.call(rbind, data)
  
  data
}

saveDataGsheets <- function(data, storage) {
  gs_add_row(gs_key(storage$key), input = data)
}
loadDataGsheets <- function() {
  gs_read_csv(gs_key(storage$key))
}

#' @export
formUI <- function(formInfo) {
  
  ns <- NS(formInfo$id)
  
  questions <- formInfo$questions
  
  fieldsMandatory <- Filter(function(x) { !is.null(x$mandatory) && x$mandatory }, questions)
  fieldsMandatory <- unlist(lapply(fieldsMandatory, function(x) { x$id }))
  
  tagList(
    shinyjs::useShinyjs(debug=T),
    shinyjs::inlineCSS(appCSS),
    div(
      id = ns("form"),
      lapply(
        questions,
        function(question) {
          label <- question$title
          if (question$id %in% fieldsMandatory) {
            label <- labelMandatory(label)
          }
          
          if (question$type == "text") {
            textInput(ns(question$id), label, "")
          } else if (question$type == "numeric") {
            numericInput(ns(question$id), label, 0)
          } else if (question$type == "checkbox") {
            checkboxInput(ns(question$id), label, FALSE)
          }
        }
      ),
      actionButton(ns("submit"), "Submit", class = "btn-primary"),
      shinyjs::hidden(
        span(id = ns("submit_msg"), class = "sf_submit_msg", "Submitting..."),
        div(class = "sf_error", id = ns("error"),
            div(br(), tags$b("Error: "), span(id = ns("error_msg")))
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("thankyou_msg"),
        h3("Thanks, your response was submitted successfully!"),
        actionLink(ns("submit_another"), "Submit another response")
      )
    )
  )
}

#' @export
formServer <- function(formInfo) {
  callModule(formServerHelper, formInfo$id, formInfo)
}

formServerHelper <- function(input, output, session, formInfo) {
  questions <- formInfo$questions
  
  fieldsMandatory <- Filter(function(x) { x$mandatory }, questions)
  fieldsMandatory <- unlist(lapply(fieldsMandatory, function(x) { x$id }))
  fieldsAll <- unlist(lapply(questions, function(x) { x$id }))
  
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  # When the Submit button is clicked, submit the response
  observeEvent(input$submit, {
    
    # User-experience stuff
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    on.exit({
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
    
    # Save the data (show an error message in case of error)
    tryCatch({
      saveData(formData(), formInfo$storage)
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    },
    error = function(err) {
      shinyjs::logjs(err)
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    })
  })
  
  if (!is.null(formInfo$multiple) && !formInfo$multiple) {
    submitMultiple <- FALSE
    shinyjs::hide("submit_another")
  } else {
    submitMultiple <- TRUE
  }
  observeEvent(input$submit_another, {
    if (!submitMultiple) {
      return()
    }
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })
  
  # Gather all the form inputs (and add timestamp)
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = as.integer(Sys.time()))
    data <- t(data)
    data
  }) 
  
  
}