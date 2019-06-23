
library(shiny)

context("Testing formUI Func")


test_that("formInfo param is a list", {
  formInfo <- data.frame(test="")
  expect_error(createFormApp(formInfo))
})


