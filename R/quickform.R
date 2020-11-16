#' Quickly create a Google Form look-a-like Shiny app
#'
#' @description Create a shiny app with minimal code that mimicks the look of a Google Form. 
#'  Currently only stores data in Google Drive (or locally) and has the option to allow survey participants to return and edit their survey by providing a unique ID.
#'  The IDs can also be emailed to the user (see "emailId").
#'  Use of remote storage with Google Drive or emailing IDs requires one-time interactive setup of {googledrive}, {googlesheets4}, and {gmailr} (if applicable). 
#'  This function has little flexibility/customize-ability, but offers quick and easy setup that reduces the time needed between developing and deploying a survey.
#'  \href{https://brentscott93.shinyapps.io/quickform/}{Live Demo}
#'  
#'  This is basically a wrapper around the usual shinyApp(ui, server) workflow that exists in an app.R file so there is no need to write any Shiny code or even explicitly call library(shiny). 
#'  quickform() will handle all of that.. 
#'  As a result, a quickform() app can only exist as a standalone app.R file.
#'  
#' @param title a character string. Title of the form/survey.
#' @param description a character string. A description providing more information about the form/survey.
#' @param questions a nested list of questions. Must contain 'id', 'type', 'question', and 'choices' (depending on the type of input) in each list element. The widget types are based on the naming from Google Forms and must be one of the following: 
#' "numeric", "checkbox", "multiplechoice", "dropdown", "paragraph", "shortanswer".  See example.
#' @param gmail either FALSE to save data locally or your gmail account to store data in Google Drive and optionally to email IDs to users. 
#' Each response is saved as an individual Google Sheet and is saved in the folder specified in the 'folder' argument. 
#' Google Drive/Sheets authorization is cached in the shiny app directory in '.secrets'. Make sure to upload this file when deploying to a server (like shinyapps.io).
#' Similarily, gmailr authorization is stored in '.gm-secrets'.
#' @param folder a character string specifying the folder on desktop/Google Drive to store results in.
#' @param returningUser logical. Do you want provide users an ID# in order to return and edit/update their survey? Default is FALSE.
#' @param emailId logical.  Do you want to email ID to users?  Only implemented if "returningUser=T" and valid email is given to "gmail" argument.
#'  Default is FALSE.
#'  If TRUE, need to setup gmail credentials and move the json file into quickform/shiny app directory as 'credentials.json'.
#'  \href{https://github.com/r-lib/gmailr}{More info on gmailr package and setup}. 
#' @param subject a character string. For the subject of your email. Default is 'Your survey ID'. Only used if "emailId=T".
#' @param color a character string specifying a hex color or to theme the app. Default is blue ("#2e77ff"). Header of title box and submit button are the actual color and background is made semi-transparent `with scales::alpha(color, 0.5)`.
#'
#' @return A Shiny App
#' @export
#' @import shiny gmailr
#' @examples
#' if(interactive()){
#' library(shinyforms)
#' quickform(
#' title = "My Survey",
#' description = "Describe your survey here",
#' questions = list(
#'   list(id = "name",
#'        type = "shortanswer",
#'        question = "What is your name?"),
#'   list(id = "age",
#'        type = "numeric",
#'        question = "Age (yrs)",
#'        required = TRUE),
#'   list(id = "ethnicity",
#'        type = "multiplechoice",
#'        question = "Are you of Hispanic, Latino, or of Spanish origin?" ,
#'        choices = list("No", "Yes"),
#'        required = TRUE),
#'   list(id = "shortanswer",
#'        type = "shortanswer",
#'        question = "One word to describe this app",
#'        required = TRUE),
#'   list(id = "user_opinion",
#'        type = "paragraph",
#'        question= "Please provide any feedback")
#' ),
#' gmail = FALSE,
#' folder = "shinyforms"
#' )
#' }
quickform <- function(title = NULL,
                      description = NULL,
                      questions = NULL,
                      gmail = FALSE,
                      folder = "shinyforms",
                      returningUser = FALSE,
                      emailId = FALSE,
                      subject = "Your survey ID",
                      color = "#2e77ff"){
  
  # check for correct user inputs
  checkmate::assertString(title)
  checkmate::assertString(description)
  checkmate::assertList(questions, min.len = 1)
  lapply(questions, checkmate::assertList, min.len = 3) #"Every question list needs at least 3 named elements 'id', 'type', and 'question'"
  if(gmail != FALSE) checkmate::assertString(gmail, pattern = "@")
  checkmate::assertLogical(returningUser)
  checkmate::assertLogical(emailId)
  checkmate::assertString(subject)
  checkmate::assertString(color)
 
  # CSS to use for app
  quickformCSS <- paste0("body { 
                                 background-color:",  scales::alpha(color, 0.5), "; 
                               }
                         
                         .quickform-title { 
                                            border-radius: 3px; 
                                            margin-top: 8px;
                                            background: white;  
                                            border-top: 3px solid", color, ";  
                                            margin-bottom: 20px; 
                                            width: 100%; 
                                            box-shadow: 0 1px 1px rgba(0, 0, 0, 0) 
                                          }
  
                          .quickform { 
                                       border-radius: 3px; 
                                       margin-top: 4px;
                                       background: white;  
                                       border-top: 3px solid white;  
                                       margin-bottom: 20px;
                                       width: 100%; 
                                       box-shadow: 0 1px 1px rgba(0, 0, 0, 0);
                                     }
                                              
                           .header { 
                                      height: 40px;
                                      font-size: 17px;
                                      padding: 8px;
                                   }
                                    
                           .body {  
                                   margin-top: 0;
                                   overflow: auto;
                                   font-size: 14px;
                                   padding: 8px;
                                   width: 100%;
                                  }
                                          
                           * { 
                               font-family: 'Source Sans Pro', 'Helvetica Neue', Helvetica, Arial, sans-serif;
                               color: #444 
                             }
                         ")
  
  # setup storage locations and authentications to google services
  if(gmail == FALSE){
    dir.create(folder, showWarnings = FALSE)
  } else {
      googledrive::drive_auth(email = gmail, cache = '.secrets')
      googlesheets4::gs4_auth(token = googledrive::drive_token())
      # Create new sheets folder for responses
      find_folder <- googledrive::drive_find(folder, n_max = 10)
      find_folder <- find_folder[find_folder[["name"]] == folder,]
      if(nrow(find_folder) == 0){
        googledrive::drive_mkdir(folder)
      }
  }
  # if the user wants the participants to be able to allow people to edit/return to update survey
  # option to email unique id with {gmailr}
  if(returningUser){
    if(emailId){
      json_file <- list.files(pattern = 'credentials.json')
        if(identical(json_file, character(0))){
          stop('You have selected to use your gmail to send emails, but there are no credentials in your app directory.')
        }
      # Configure your app
      gmailr::gm_auth_configure(path = "credentials.json")
      # Authenticate with the tokens in the copied cache
      gmailr::gm_auth(email = gmail, cache = '.gm-secrets')
    }}
  
  #user interface - mimics looks of a google form
  ui <- fluidPage(
          id = 'app',
            shinyjs::useShinyjs(),
            shinyjs::inlineCSS(quickformCSS),
                column(3),
                column(6,
                  titleBox(title, description),
                  uiOutput('returningUser'),
                  lapply(questions, createQuestion),
                  actionButton('submit',
                               'Submit',
                                style = paste0("background-color: ", color, "; color:#fff;"))
                     ) # column close
                   )
  
  server <- function(input, output, session) {
    
    rv <- reactiveValues(saved = 0)
    observeEvent(input[["submit"]], {
      #check if returning user has entered a valid ID 
      if(returningUser){
        if(input[["user"]] == "return"){
          if(input[["userId"]] == "") showNotification("Please enter ID if you are a returning user.", type = "error")
          req(input[["userId"]])
          if(nchar(input[["userId"]]) != 21) showNotification("Not a valid ID. Must be 21 characters", type = "error")
          req(nchar(input[["userId"]]) == 21)
        }
      }
     #check to see if any required values are NULL - if so stop reactivity and show error informing user to answer all required
      lapply(questions, checkRequired, input = input)
      
      withProgress(message = "Saving", {
        setProgress(0.2)
        responses <- lapply(questions, getUserInput, input = input)
        setProgress(0.35)
        data <- do.call("cbind", responses)
        filename <- paste0(gsub( "[^[:alnum:]]", "", Sys.time()), round(stats::runif(1, 1000000, 2000000)))
        setProgress(0.5)
        if(returningUser){
          if(input[["user"]] == "new"){
            rv[["filename"]] <- filename
            data[["id"]] <- filename
            if(gmail == FALSE){
              utils::write.csv(data,
                               file.path(folder, paste0(filename, ".csv")),
                               row.names = FALSE)
            } else {
              saveToDrive(data = data, filename = filename, folder = folder)
            }
          } else if(input[["user"]] == "return"){
             #if is returning user and local storage write to disk
            rv[["filename"]] <- input[["userId"]]
              if(gmail == FALSE){
                utils::write.csv(data,
                                 file = file.path(folder, paste0(input[["userId"]], ".csv")),
                                 row.names = FALSE)
              } else {
                userDataOverwrite <- googledrive::drive_find(input[["userId"]], n_max = 1)
                #check to make sure user supplied ID matches a file on drive
                if(nrow(userDataOverwrite) == 0){
                  showNotification("Not a valid ID", type = "error")
                 } else {
                  #if ID matches a file overwrite with new survey data
                  data[["id"]] <- input[["userId"]]
                  googlesheets4::write_sheet(data, ss = userDataOverwrite, sheet = 'data')
                }
             } 
          } 
        } else { #if non returningUser
          if(gmail == FALSE){
            utils::write.csv(data, 
                             file = file.path(folder, paste0(filename, ".csv")),
                             row.names = FALSE)
          } else {
            saveToDrive(data = data, filename = filename, folder = folder)
         }
        }
      setProgress(1)
    })
      #once complete -  update a reactive value to launch a modal
      rv[["saved"]] <- rv[["saved"]] + 1
      shinyjs::reset("app")
    })
    
    output[["showId"]] <- renderPrint({
      cat(rv[["filename"]])
    })
    
    observeEvent(rv[["saved"]], ignoreInit = TRUE, {
      if(returningUser){
        shiny::showModal(
          shiny::modalDialog(
          title = "Response Saved!",
          "Your response has been saved. You can return to this survery and update your answers by entering in the following ID in the returning user section at the top of the form: ",
          br(),
          verbatimTextOutput("showId"),
          br(),
          #if user wants to email unique IDs to client show an email textInput
          if(emailId){
            textInput("address",
                      label = "Would you like this emailed?",
                      placeholder = "yourEmail@here.com")
          },
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "OK")
          )
        ))
      } else {
        shiny::showModal(
          shiny::modalDialog(
            title = "Response Saved",
            "Thanks for participating!",
          ))
      }
    })
    
    observeEvent(input[["ok"]], ignoreInit = TRUE, {
      #if ok is clicked in modal dialogue and a valid email is entered - email id to address with gmailr if emailId is T
      if(emailId){
        if(grepl('@', input[["address"]])){
          email <- gmailr::gm_mime() %>%
            gmailr::gm_to(input[["address"]]) %>%
            gmailr::gm_from(gmail) %>%
            gmailr::gm_subject(subject) %>%
            gmailr::gm_text_body(paste0("Thanks for taking the survey. Your returning ID is: " , rv[["filename"]]))
          
          gmailr::gm_send_message(email)
          shiny::showNotification("Email sent!", type = "message")
          removeModal()
            
          } else {
             showNotification("Not a valid email address", type = "error")
            }
        
      } else {
        #if emailId is False both Ok and cancel remove the modal
        removeModal()
      }
    })
    
    #### Returning User ####
    #makes the returning user UI box
    output[["returningUser"]] <- renderUI({
      if(returningUser){
        questionBox(
            question = "Returning User?",
            ui = shiny::tagList(radioButtons("user",
                         label = NULL,
                         choices = list("New User" = "new",
                                        "Returning" = "return"),
                         inline = TRUE),
            shiny::textInput("userId", label = "Enter ID", placeholder = "Enter ID Here"),
            shiny::actionButton("loadReturning", "Load")
            )
        )
      }
    })
    
    #reactive events that occur if a returning user ID is entered
    #the app will search for a matching filename to the unique ID
    #if one is found the survey is updated with those results
    observeEvent(input[["loadReturning"]], {
      if(returningUser){
        if(input[["user"]] == "return"){
          if(input[["userId"]] == "") shiny::showNotification("Please enter ID if you are a returning user.")
          req(input[["userId"]])
          withProgress(message = "Loading",{
            if(gmail != FALSE){
              #googledrive storage
              userFile <-  googledrive::drive_find(input[["userId"]], n_max = 1)
              if(nrow(userFile) == 0) showNotification("No file matches that ID", type = "error")
              req(nrow(userFile) == 1)
              setProgress(0.5, detail = "Updating")
              data <- googlesheets4::read_sheet(userFile)
            } else if(gmail == FALSE){
              #local file storage
              userFile <- list.files(folder, pattern = input[["userId"]], full.names = TRUE)
              if(identical(userFile, character(0))) showNotification("No file matches that ID", type = "error")
              req(!identical(userFile, character(0)))
              data <- utils::read.csv(userFile)
            }
            #loop through every questions list element
            #update the shiny widget with the saved value in the google sheet
            for(i in seq_along(questions)){
              if (questions[[i]][["type"]] == "numeric"){
                updateNumericInput(session = session,
                                   inputId = questions[[i]][["id"]],
                                   value =  data[[questions[[i]][["id"]]]])
                
              } else if (questions[[i]][["type"]] == "checkbox"){
                updateCheckboxInput(session = session,
                                    inputId = questions[[i]][["id"]],
                                    value =  data[[questions[[i]][["id"]]]])
                
              } else if (questions[[i]][["type"]] == 'multiplechoice'){
                updateRadioButtons(session = session,
                                   inputId =  questions[[i]][["id"]],
                                   selected =  data[[questions[[i]][["id"]]]])
                
              } else if (questions[[i]][["type"]] == 'dropdown'){
                updateSelectInput(session = session,
                                  inputId =  questions[[i]][["id"]],
                                  selected =  data[[questions[[i]][["id"]]]])
                
              } else if (questions[[i]][["type"]] == 'shortanswer'){
                updateTextInput(session = session,
                                inputId =  questions[[i]][["id"]],
                                value =  data[[questions[[i]][["id"]]]])
                
              } else if (questions[[i]][["type"]] == 'paragraph'){
                updateTextAreaInput(session = session,
                                    inputId = questions[[i]][["id"]],
                                    value =  data[[questions[[i]][["id"]]]])
                
              }
            }
          })
          showNotification("Survey Loaded", type = "message")
        }
      }
    })
    
    #user-experience with shinyjs
    observeEvent(input[["user"]], {
      if(input[["user"]] == 'return'){
        shinyjs::show("userId")
        shinyjs::show("loadReturning")
        
      } else {
        shinyjs::hide("userId")
        shinyjs::hide("loadReturning")
      }
    })
  }
  
  shinyApp(ui, server)
  
}
