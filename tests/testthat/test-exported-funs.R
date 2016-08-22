library(panelPomp)

context("For now, the context is the name of the function being tested (along with the file it is in)")

test_that("functionality X is being implemented as intended", {
  # When "functionality X is being implemented as intended," then one should ...
  expect_equal(
    object = 10, 
    expected = 10 + 1e-7) 
  expect_identical(# THIS FAILS #expect_identical(10, 10 + 1e-7)
    object = 10, 
    expected = 10); string <- "Testing is fun!"
  expect_match(
    object = string, 
    regexp = "Testing is fun!")
  expect_match(# additional arguments are passed to grepl 
    object = string, 
    regexp = "testing", 
    ignore.case = TRUE); a <- list(1:10, letters)
  expect_output(
    object = str(a), 
    regexp = "List of 2")
  expect_output(# additional arguments are passed to grepl
    object = str(a), 
    regexp = "int [1:10]", 
    fixed = TRUE)
  #expect_message(
  #  object = pomp::pompExample("panelGompertz"), 
  #  "Newly created object(s):") # The message is part of the ... argument
  expect_warning(# leaving the second argument blank will produce an error with the actual message/warning
    "NaNs produced", # The message is part of the ... argument
    object = log(-1)); pomp::pompExample("panelGompertz")
  expect_is(
    object = panelGompertz, 
    class = "panelPomp")
  expect_true(
    object = TRUE==TRUE)
  expect_false(
    object = TRUE==FALSE)
  #expect_equal_to_reference(
  #  object = coef(panelGompertz), 
  #  file = "exported-funs.rds") # must be an .rds file
})
