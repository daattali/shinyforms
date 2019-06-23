
library(shiny)

context("Testing formServer Func")


test_that("formServer param is a list", {
  formInfo <- data.frame(test="")
  expect_error(formServer(formInfo))
})


