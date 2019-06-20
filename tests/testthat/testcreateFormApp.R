
library(shiny)

context("Testing CreateFormApp Func")

test_that("formInfo param is Not Null", {
  formInfo <- NULL
  expect_error(createFormApp(formInfo))
})



test_that("formInfo param is a list", {
  formInfo <- data.frame(test="")
  expect_error(createFormApp(formInfo))
})
