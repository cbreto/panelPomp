library(panelPomp)

context("test-misc-exported")

test_that("panel_logmeanexp runs", {
  # DEBUG
  x <-
    array(
      data = c(log(1e-310), log(1e-220), log(1e-308), log(1e-300)),
      dim = c(2, 2),
      dimnames = list(unit = paste0("unit", 1:2), rep = NULL)
    )#; x
  MARGIN <- 1
  y <- unname(panel_logmeanexp(x = x, MARGIN = MARGIN, se = TRUE)[1])
  z <- unname(panel_logmeanexp(x = x, MARGIN = MARGIN, se = FALSE))
  
  expect_true(object = identical(x = -1217.141, y = round(y, 3)))
})

pompExample(panelGompertz)
pompExample(panelGompertzShared)
pompExample(panelGompertzSpecific)
listPparams.shared.only <- coef(panelGompertzShared)
listPparams.specific.only <- coef(panelGompertzSpecific)
listPparams <- coef(panelGompertz)

test_that("from/toVectorPparams work (shared parameters only)", {
  res <- fromVectorPparams(toVectorPparams(listPparams.shared.only))
  expect_true(object = identical(x = res, y = listPparams.shared.only))
})

test_that("from/toVectorPparams work (specific parameters only)", {
  res <- fromVectorPparams(toVectorPparams(listPparams.specific.only))
  expect_true(object = identical(x = res, y = listPparams.specific.only))
})

test_that("from/toVectorPparams work (both shared & specific parameters)", {
  res <- fromVectorPparams(toVectorPparams(listPparams))
  expect_true(object = identical(x = res, y = listPparams))
})