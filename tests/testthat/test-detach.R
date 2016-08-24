library(panelPomp)

context("Test")

test_that("pompExamples changes when panelPomp is detached", {
  require(panelPomp)
  detach(package:panelPomp)
})