library(testthat)
library(shinyforms)

#testing the quickform() function and helpers

test_that("getUserInput() subsets a list and returns df", {
  question <- list(id = "test_id")
  input <- list(test_id = "test")
  expect_equal(shinyforms:::getUserInput(question, input), data.frame(test_id = "test"))
})

test_that("quickform() errors when char. string not supplied to 'title' arg", {
  expect_error(quickform(title = 1), "string")
})

test_that("quickform() errors when char. string not supplied to 'description' arg", {
  expect_error(quickform(title = "1", description = 1), "string")
})

test_that("quickform() errors with invalid email", {
  expect_error(quickform(questions = list(list(id = "id", type = "numeric")), gmail = "email"), "string")
})

test_that("quickform() errors when questions is not a list", {
  expect_error(quickform(questions = "string", title = "", description = ""), "list")
})

test_that("quickform() errors when questions element is not a list", {
  expect_error(quickform(title = "", 
                         description = "",
                         questions = list(
                                       list(id = "id", type = "numeric"))),
               "Must have length >= 3")
})

test_that("createQuestion() returns shiny.tag", {
  expect_equal(class(shinyforms:::createQuestion(list(type="numeric", id = "id", question = "?"))), "shiny.tag")
})

test_that("createQuestion() errors with invalid type", {
  expect_error(shinyforms:::createQuestion(list(id = "id", type = "invalid", question = "?")), 'Not a valid')
})

test_that("createQuestion() errors with no id", {
  expect_error(shinyforms:::createQuestion(list(type = 'invalid')), "string")
})

test_that("createQuestion() errors with no question", {
  expect_error(shinyforms:::createQuestion(list(type = 'numeric', id = 'id')), "string")
})

