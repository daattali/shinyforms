library(shiny)
library(shinyforms)

# Define the first form: basic information
basicInfoForm <- list(
  id = "basicinfo",
  questions = list(
    list(id = "name", type = "text", title = "Name", mandatory = TRUE),
    list(id = "age", type = "numeric", title = "Age", mandatory = FALSE),
    list(id = "date", type = "date", title = "Date", mandatory = FALSE),
    list(id = "slider", type = "slider", title = "Slider", mandatory = FALSE, min = 1,max = 10,value = 1),
    list(id = "favourite_pkg", type = "text", title = "Favourite R package"),
    list(id = "terms", type = "checkbox", title = "I agree to the terms")
  ),
  storage = list(
    type = STORAGE_TYPES$FLATFILE,
    path = "responses"
  ),
  name = "Personal info",
  password = "shinyforms"
)

# Define the second form: soccer
soccerFormInfo <- list(
  id = "soccerform",
  questions = list(
    list(id = "team", type = "text", title = "Favourite soccer team"),
    list(id = "player", type = "text", title = "Favourite player"),
    list(id = "league", type = "text", title = "Favourite league")
  ),
  storage = list(
    type = STORAGE_TYPES$FLATFILE,
    path = "soccer"
  ),
  multiple = FALSE
)
forms <- list(basicInfoForm,soccerFormInfo)
ui <- fluidPage(
  h1("shinyforms example"),
  tabsetPanel(
    tabPanel(
      "Basic info",
      formUI(forms)
    ),
    tabPanel(
      "Soccer"
      #,
      # formUI(soccerFormInfo)
    )
  )
)

server <- function(input, output, session) {
  formServer(basicInfoForm)
  formServer(soccerFormInfo) 
}

shinyApp(ui = ui, server = server)
