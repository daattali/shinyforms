

library(shiny)

context("Testing saveDataFlatfile Func")


test_that("check saveDataFlatfile outputs fileName when fileName is specified", {
  
  # create data input
  data <- data.frame(name = "", age = 0, favourite_pkg = "", terms = TRUE)
  path = getwd()
  
  # create storage input
  storage = list(
    type = STORAGE_TYPES$FLATFILE,
    fileName = "TestingExample",
    path = path
  )
  
  # use saveDataFlatfile function
  saveDataFlatfile(data, storage)
  fileName <- file.path(path, "TestingExample.csv")
  
  # test what we expect against the resulting csv
  expect_equal(fileName, paste0(path, '/', dir(path=path, pattern="*csv")[1]))
  
  ## Clean up
  files <- paste0(path, '/', dir(path=path, pattern="*csv*"))
  file.remove(files)
  
})





test_that("check saveDataFlatfile outputs fileName when fileName is not specified", {
  
  # create data input
  data <- data.frame(name = "", age = 0, favourite_pkg = "", terms = TRUE)
  path = getwd()
  
  # create storage input
  storage = list(
    type = STORAGE_TYPES$FLATFILE,
    path = path
  )
  
  # use saveDataFlatfile function
  saveDataFlatfile(data, storage)

  
  # test what we expect against the resulting csv
  expect_equal(52,   nchar(dir(path=path, pattern="*csv")[1]))
  
  ## Clean up
  files <- paste0(path, '/', dir(path=path, pattern="*csv*"))
  file.remove(files)
  
})
