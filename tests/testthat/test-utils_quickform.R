library(testthat)
library(shinyforms)

#testing the quickform() helper files

test_that("saveResponse() subsets a list and returns df", {
  question <- list(id = 'test_id')
  input <- list(test_id = 'test')
  expect_equal(shinyforms:::saveResponse(question, input), data.frame(test_id = 'test'))
})

test_that("quickform() errors with invalid email", {
  expect_error(quickform(gmail = 'email'), 'Not a valid')
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

