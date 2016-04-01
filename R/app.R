questions <- list(
  list(id = "name", type = "text", title = "Name", mandatory = TRUE),
  list(id = "age", type = "numeric", title = "Name", mandatory = FALSE),
  list(id = "favourite_pkg", type = "text", title = "Favourite R package", mandatory = FALSE),
  list(id = "terms", type = "checkbox", title = "I agree to the terms")
)
formInfo <- list(
  id = "form1",
  questions = questions,
  storage = list(
    type = STORAGE_TYPES$FLATFILE,
    path = "responses"
  )
)


formInfo2 <- list(
  id = "form2",
  multiple = FALSE,
  questions = list(
    list(id = "team", type = "text", title = "Favourite soccer team"),
    list(id = "player", type = "text", title = "Favourite player")
  ),
  storage = list(
    #type = STORAGE_TYPES$GOOGLE_SHEETS,
    #key = "1PQuXmzr-6Y9r-m5P7jtJcg2Z3WJ7SyzrLLYbBOmsPys"
    type = STORAGE_TYPES$FLATFILE,
    path = "soccer"
  )
)

ui <- fluidPage(
  h1("rOpenSci shinyforms"),
  tabsetPanel(
    tabPanel(
      "Tab 1",
      formUI(formInfo)
    ),
    tabPanel(
      "Tab 2",
      formUI(formInfo2)
    )
  )
)

server <- function(input, output, session) {
  formServer(formInfo)
  formServer(formInfo2)
}

shinyApp(ui = ui, server = server)



