library(shiny)

# Test whether a given object is a valid non-empty list
# @param listname a potential list to verify
# returns TRUE if the given object is a non-empty list, FALSE otherwise
testList <- function(listname){
  return(!is.null(listname) && 
         length(listname) != 0 &&
           "list" %in% class(listname))
}


# A list of all the available storage types for shinyforms.
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



# Adds a mandatory star to a labelled question.
# @param label A string representing the mandatory question.
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}



# shinyform app defined CSS format
appCSS <- "
.shinyforms-ui .mandatory_star { color: #db4437; font-size: 20px; line-height: 0; }
.shinyforms-ui .sf-questions { margin-bottom: 30px; }
.shinyforms-ui .sf-question { margin-top: 25px; font-size: 16px; }
.shinyforms-ui .question-hint { font-size: 14px; color: #737373; font-weight: normal; }
.shinyforms-ui .action-button.btn { font-size: 16px; margin-right: 10px; }
.shinyforms-ui .thankyou_msg { margin-top: 10px; }
.shinyforms-ui .showhide { margin-top: 10px; display: inline-block; }
.shinyforms-ui .sf_submit_msg { font-weight: bold; }
.shinyforms-ui .sf_error { margin-top: 15px; color: red; }
.shinyforms-ui .answers { margin-top: 25px; }
.shinyforms-ui .pw-box { margin-top: -20px; }
.shinyforms-ui .created-by { font-size: 12px; font-style: italic; color: #777; margin: 25px auto 10px;}
"




# Takes data from your shinyforms inputs and passes it to a storage type.
# @param data Dataframe taken from input shiny object.
# @param storage A list with variable type defining users perferred type of storage
saveData <- function(data, storage) {
  if (storage$type == STORAGE_TYPES$FLATFILE) {
    saveDataFlatfile(data, storage)
  } else if (storage$type == STORAGE_TYPES$GOOGLE_SHEETS) {
    saveDataGsheets(data, storage)
  }
}




# Passes data from a storage type and passes it back to shiny. 
# Currently only provides storage of flat files (.csv).
# @param storage A list with variable type defining users perferred type of storage.
loadData <- function(storage) {
  if (storage$type == STORAGE_TYPES$FLATFILE) {
    loadDataFlatfile(storage)
  } else if (storage$type == STORAGE_TYPES$GOOGLE_SHEETS) {
    #loadDataGsheets(storage)
  }
}



# Takes data from your shinyforms inputs and saves it to a flat file. 
# Writes form inputs to a storage type and names it using a timestamp.
# @param data Dataframe taken from input shiny object
# @param storage A list with variable type defining users perferred type of storage and storage path
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



# Takes data from a flat file and passes it to your shiny app.
# @param storage A list with variable type defining users perferred type of storage
loadDataFlatfile <- function(storage) {
  resultsDir <- storage$path
  files <- list.files(file.path(resultsDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  data <- do.call(rbind, data)
  
  data
}



# Takes data from your shinyforms inputs and saves it to a google doc file
# @param data Dataframe taken from input shiny object
# @param storage A list with variable type defining users perferred type of storage and storage key
saveDataGsheets <- function(data, storage) {
  gs4_add_row(gs4_key(storage$key), input = data)
}



# Takes data from a google doc file and passes it to your shiny app.
# @param storage A list with variable type defining users perferred type of storage and storage key
loadDataGsheets <- function() {
  gs4_read_csv(gs4_key(storage$key))
}



#' Creates the UI form component for shinyforms. 
#'
#' @param formInfo A list with param: id, questions and storage 
#' 
#' @examples  
#' if (interactive()) {
#' library(shiny)
#' library(shinyforms)
#'
#' questions <- list(
#'   list(id = "name", type = "text", title = "Name", mandatory = TRUE),
#'   list(id = "age", type = "numeric", title = "Age"),
#'   list(id = "favourite_pkg", type = "text", title = "Favourite R package"),
#'   list(id = "terms", type = "checkbox", title = "I agree to the terms")
#' )
#' formInfo <- list( 
#' id = "basicinfo",
#' questions = questions,
#' storage = list(
#'   # Right now, only flat file storage is supported
#'   type = STORAGE_TYPES$FLATFILE,
#'   # The path where responses are stored
#'   path = "responses"
#' )
#' )
#' ui <- fluidPage(
#'   formUI(formInfo)
#' )
#'
#' server <- function(input, output, session) {
#'   formServer(formInfo)
#' }
#'
#' shinyApp(ui = ui, server = server)
#'}
#' @export
formUI <- function(formInfo) {
  if (!testList(formInfo)) {
    stop("`formInfo` is not a valid list")
  }
  
  ns <- NS(formInfo$id)
  
  questions <- formInfo$questions
  
  fieldsMandatory <- Filter(function(x) { !is.null(x$mandatory) && x$mandatory }, questions)
  fieldsMandatory <- unlist(lapply(fieldsMandatory, function(x) { x$id }))
  
  titleElement <- NULL
  if (!is.null(formInfo$name)) {
    titleElement <- h2(formInfo$name)
  }
  
  responseText <- "Thank you, your response was submitted successfully."
  if (!is.null(formInfo$responseText)) {
    responseText <- formInfo$responseText
  }
  
  div(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    class = "shinyforms-ui",
    div(
      id = ns("form"),
      titleElement,
      div(
        class = "sf-questions",
        lapply(
          questions,
          function(question) {
            label <- question$title
            if (question$id %in% fieldsMandatory) {
              label <- labelMandatory(label)
            }
            
            if (question$type == "text") {
              input <- textInput(ns(question$id), NULL, "")
            } else if (question$type == "numeric") {
              input <- numericInput(ns(question$id), NULL, 0)
            } else if (question$type == "checkbox") {
              input <- checkboxInput(ns(question$id), label, FALSE)
            }

            div(
              class = "sf-question",
              if (question$type != "checkbox") {
                tags$label(
                  `for` = ns(question$id),
                  class = "sf-input-label",
                  label,
                  if (!is.null(question$hint)) {
                    div(class = "question-hint", question$hint)
                  }
                )
              },
              input
            )
          }
        )
      ),
      actionButton(ns("submit"), "Submit", class = "btn-primary"),
      if (!is.null(formInfo$reset) && formInfo$reset) {
        actionButton(ns("reset"), "Reset")
      },
      shinyjs::hidden(
        span(id = ns("submit_msg"),
             class = "sf_submit_msg",
             "Submitting..."),
        div(class = "sf_error", id = ns("error"),
            div(tags$b(icon("exclamation-circle"), "Error: "),
                span(id = ns("error_msg")))
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("thankyou_msg"),
        class = "thankyou_msg",
        strong(responseText), br(),
        actionLink(ns("submit_another"), "Submit another response")
      )
    ),
    shinyjs::hidden(
      actionLink(ns("showhide"),
                 class = "showhide",
                 "Show responses")
    ),
    
    shinyjs::hidden(div(
      id = ns("answers"),
      class = "answers",
      div(
        class = "pw-box", id = ns("pw-box"),
        inlineInput(
          passwordInput(ns("adminpw"), NULL, placeholder = "Password")
        ),
        actionButton(ns("submitPw"), "Log in")
      ),
      shinyjs::hidden(div(id = ns("showAnswers"),
          downloadButton(ns("downloadBtn"), "Download responses"),
          DT::dataTableOutput(ns("responsesTable"))
      ))
    )),
    
    div(class = "created-by",
        "Created with",
        a(href = "https://github.com/daattali/shinyforms", "shinyforms")
    )
  )
}




#' Creates the server component for shinyforms
#'
#' @param formInfo A list with param: id, questions and storage
#' 
#' @examples 
#' if (interactive()) {
#' library(shiny)
#' library(shinyforms)
#'
#' questions <- list(
#'   list(id = "name", type = "text", title = "Name", mandatory = TRUE),
#'   list(id = "age", type = "numeric", title = "Age"),
#'   list(id = "favourite_pkg", type = "text", title = "Favourite R package"),
#'   list(id = "terms", type = "checkbox", title = "I agree to the terms")
#' )
#' formInfo <- list(
#' id = "basicinfo",
#' questions = questions,
#' storage = list(
#'   # Right now, only flat file storage is supported
#'   type = STORAGE_TYPES$FLATFILE,
#'   # The path where responses are stored
#'   path = "responses"
#' )
#' )
#' ui <- fluidPage(
#'   formUI(formInfo)
#' )
#'
#' server <- function(input, output, session) {
#'   formServer(formInfo)
#' }
#'
#' shinyApp(ui = ui, server = server)
#' } 
#' @export
formServer <- function(formInfo) {
  if (!testList(formInfo)) {
    stop("`formInfo` is not a valid list")
  }
  callModule(formServerHelper, formInfo$id, formInfo)
}




# Helper function for formServer component
formServerHelper <- function(input, output, session, formInfo) {
  if (grepl("\\s", formInfo$id)) {
    stop("Form id cannot have any spaces", call. = FALSE)
  }
  
  if (formInfo$storage$type == STORAGE_TYPES$FLATFILE) {
    if (!dir.exists(formInfo$storage$path)) {
      dir.create(formInfo$storage$path, showWarnings = FALSE)
    }
  }
  
  questions <- formInfo$questions
  
  fieldsMandatory <- Filter(function(x) {!is.null(x$mandatory) && x$mandatory }, questions)
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
  
  observeEvent(input$reset, {
    shinyjs::reset("form")
    shinyjs::hide("error")
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

    if (!is.null(formInfo$validations)) {
      errors <- unlist(lapply(
        formInfo$validations, function(validation) {
          if (!eval(parse(text = validation$condition))) {
            return(validation$message)
          } else {
            return()
          }
        }
      ))
      if (length(errors) > 0) {
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
        if (length(errors) == 1) {
          shinyjs::html("error_msg", errors[1])  
        } else {
          errors <- c("", errors)
          shinyjs::html("error_msg", paste(errors, collapse = "<br>&bull; "))
        }
        return()
      }
    }
    
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
  
  output$responsesTable <- DT::renderDataTable({
    if (!values$adminVerified) {
      return(matrix(0))
    }
    
    DT::datatable(
      loadData(formInfo$storage),
      rownames = FALSE,
      options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE)
    )
  })
  
  values <- reactiveValues(admin = FALSE, adminVerified = FALSE)
  observe({
    search <- parseQueryString(session$clientData$url_search)
    if ("admin" %in% names(search) && !is.null(formInfo$password)) {
      values$admin <- TRUE
      shinyjs::show("showhide")
    }
  })
  
  observeEvent(input$showhide, {
    shinyjs::toggle("answers")
  })

  observeEvent(input$submitPw, {
    if (input$adminpw == formInfo$password) {
      values$adminVerified <- TRUE
      shinyjs::show("showAnswers")
      shinyjs::hide("pw-box")
    }
  })

  # Allow admins to download responses
  output$downloadBtn <- downloadHandler(
    filename = function() {
      sprintf("%s_%s.csv", formInfo$id, format(Sys.time(), "%Y%m%d-%H%M%OS"))
    },
    content = function(file) {
      write.csv(loadData(formInfo$storage), file, row.names = FALSE)
    }
  )
}




# Created a yaml file for configuring shinyforms
# @param id String name of the form
# @param questions list of form questions
# @param storage a list of different storage types, path and keys
# @param name String name of the app
# @param multiple boolean 
createFormInfo <- function(id, questions, storage, name, multiple = TRUE,
                           password) {
  # as.yaml
}




#' Creates a shinyform app from the defined questions and parameters set in formInfo.
#'
#' @param formInfo A list with param: id, questions and storage
#' 
#' @examples 
#' if (interactive()) {
#' 
#' library(shiny)
#' library(shinyforms)
#' 
#' questions <- list(
#'   list(id = "name", type = "text", title = "Name", mandatory = TRUE),
#'   list(id = "age", type = "numeric", title = "Age"),
#'   list(id = "favourite_pkg", type = "text", title = "Favourite R package"),
#'   list(id = "terms", type = "checkbox", title = "I agree to the terms")
#' )
#' formInfo <- list(
#'   id = "basicinfo",
#'   questions = questions,
#'   storage = list(
#'     # Right now, only flat file storage is supported
#'     type = STORAGE_TYPES$FLATFILE,
#'     # The path where responses are stored
#'     path = "responses"
#'   )
#' )
#' 
#' createFormApp(formInfo)
#' }
#' @export
createFormApp <- function(formInfo) {
  if (!testList(formInfo)) {
    stop("`formInfo` is not a valid list")
  }
  ui <- fluidPage(
    formUI(formInfo)
  )
  server <- function(input, output, session) {
    formServer(formInfo)
  }
  shiny::shinyApp(ui = ui, server = server)
}






# Allows inline inputs to be entered i.e. for passwords and username of admins
# @param tag defined inline object
inlineInput <- function(tag) {
  stopifnot(inherits(tag, "shiny.tag"))
  tagAppendAttributes(tag, style = "display: inline-block;")
}
