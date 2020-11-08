library(testthat)
library(shinyforms)

#testing the quickform() function and helpers

test_that("getUserInput() subsets a list and returns df", {
  question <- list(id = 'test_id')
  input <- list(test_id = 'test')
  expect_equal(shinyforms:::getUserInput(question, input), data.frame(test_id = 'test'))
})

test_that("quickform() errors when char. string not supplied to 'title' arg", {
  expect_error(quickform(title = 1), 'character')
})

test_that("quickform() errors when char. string not supplied to 'description' arg", {
  expect_error(quickform(title = '1', description = 1), 'character')
})

test_that("quickform() errors with invalid email", {
  expect_error(quickform(questions = list(list(id = 'id', type = 'numeric')), gmail = 'email'), 'Not a valid')
})

test_that("quickform() errors when questions is not a list", {
  expect_error(quickform(questions = 'string'), 'must contain a list')
})

test_that("quickform() errors when questions element is not a list", {
  expect_error(quickform(questions = list(
                                       list(id = 'id', type = 'numeric'),
                                       'string')),
               'must be a list')
})

test_that('formQ() returns shiny.tag', {
  expect_equal(class(shinyforms:::formQ(list(type='numeric', id = 'id', question = '?'))), 'shiny.tag')
})

test_that("formQ() errors with invalid type", {
  expect_error(shinyforms:::formQ(list(id = 'id', type = 'invalid', question = '?')), 'Not a valid')
})

test_that("formQ() errors with no id", {
  expect_error(shinyforms:::formQ(list(type = 'invalid')), 'Every question')
})

test_that("formQ() errors with no question", {
  expect_error(shinyforms:::formQ(list(type = 'numeric', id = 'id')), 'Every question')
})

