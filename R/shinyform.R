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
.thankyou_msg { margin-top: 10px; }
.showhide { margin-top: 10px; display: inline-block; }
.sf_submit_msg { margin-left: 10px; font-weight: bold; }
.sf_error { margin-top: 15px; color: red; }
.answers { margin-top: 10px; }
.pw-box { margin-top: -20px; }
.created-by { font-size: 12px; font-style: italic; color: #777; margin: 25px auto 10px;}
"

saveData <- function(data, storage) {
  if (storage$type == STORAGE_TYPES$FLATFILE) {
    saveDataFlatfile(data, storage)
  } else if (storage$type == STORAGE_TYPES$GOOGLE_SHEETS) {
    saveDataGsheets(data, storage)
  }
}

loadData <- function(storage) {
  if (storage$type == STORAGE_TYPES$FLATFILE) {
    loadDataFlatfile(storage)
  } else if (storage$type == STORAGE_TYPES$GOOGLE_SHEETS) {
    #loadDataGsheets(storage)
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
loadDataFlatfile <- function(storage) {
  resultsDir <- storage$path
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
  formUI <- lapply(formInfo,function(form) {
    ns <- NS(form$id)
    
    questions <- form$questions
    
    fieldsMandatory <- Filter(function(x) { !is.null(x$mandatory) && x$mandatory }, questions)
    fieldsMandatory <- unlist(lapply(fieldsMandatory, function(x) { x$id }))
    
    titleElement <- NULL
    if (!is.null(formInfo$name)) {
      titleElement <- h2(formInfo$name)
    }
    
    formSheet <-  tagList(
      shinyjs::useShinyjs(),
      shinyjs::inlineCSS(appCSS),
      div(
        id = ns("form"),
        titleElement,
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
              checkboxInput(ns(question$id), label, FALSE)}
              else if (question$type == "date") {
               dateInput(ns(question$id), label, FALSE)}
            else if (question$type == "slider") {
              sliderInput(ns(question$id), label, min = question$min, max = question$max, value = question$value,
                          step = question$step)}
              
          }
        )
      )
    )
    return(formSheet)
  })
  
  submitForm <- lapply(formInfo,function(form) {
    ns <- NS(form$id)
    # if list of forms - check if max and add submit if true
    numForms <-  length(formInfo)
    maxID <- formInfo[[numForms]]$id
    if ( maxID == form$id) {
      tagList(
        shinyjs::useShinyjs(),
        shinyjs::inlineCSS(appCSS),
        div(
          actionButton(ns("submit"), "Submit", class = "btn-primary"),
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
            h3("Thanks, your response was submitted successfully!"),
            actionLink(ns("submit_another"), "Submit another response")
          )
        ),
        shinyjs::hidden(
          actionLink(ns("showhide"),
                     class = "showhide",
                     "Show responses (Admin only)")
        ),
        
        shinyjs::hidden(div(
          id = ns("answers"),
          class = "answers",
          div(class = "pw-box", id = ns("pw-box"),
              passwordInput(ns("adminpw"), "Password"),
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
    }})
  formUI <- list(formUI,submitForm)
  return(formUI)
  
}

#' @export
formServer <- function(formInfo) {
  callModule(formServerHelper, formInfo$id, formInfo)
}

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