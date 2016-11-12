library(panelPomp)

context("Test panelPomp's unexported functions")

common <- c(common.1 = 1, common.2 = 2)
u <- 5 # at least 1
specific <-
  matrix(as.numeric(paste0(rep(seq(
    3, u + 2
  ), each = 2), rep(
    c(".1", ".2"), times = u
  ))),
  nrow = 2,
  dimnames = list(c("spec.1", "spec.2") , c(paste0("unit.", 1:u)))
  )

test_that("tolistPparams and toMatrixPparams work (common parameters only)", {
  listPparams <-
    list(
      cool.common = common,
      yay.specific = array(
        data = numeric(0),
        dim = c(0, dim(specific)[2]),
        dimnames = list(NULL, dimnames(specific)[[2]])
      )
    )

  matrixPparams <- panelPomp:::toMatrixPparams(listPparams)
  vector.position.in.listPparams <- which(sapply(listPparams, is.vector))
  names.in.vector <- names(listPparams[vector.position.in.listPparams]$cool.common)
  vector.name.in.listPparams <- names(listPparams)[vector.position.in.listPparams]
  matrix.name.in.listPparams <- names(listPparams)[ifelse(vector.position.in.listPparams == 1, 2, 1)]
  
  res <- panelPomp:::toListPparams(
    matrixPparams = matrixPparams,
    names.in.vector = names.in.vector,
    vector.position.in.listPparams = vector.position.in.listPparams,
    vector.name.in.listPparams = vector.name.in.listPparams,
    matrix.name.in.listPparams = matrix.name.in.listPparams
  )
  expect_true(object = identical(x = res, y = listPparams))
})

test_that("tolistPparams and toMatrixPparams work (specific parameters only)", {
  listPparams <-
    list(cool.common = numeric(0), yay.specific = specific)
  
  matrixPparams <- panelPomp:::toMatrixPparams(listPparams)
  vector.position.in.listPparams <- which(sapply(listPparams, is.vector))
  names.in.vector <- names(listPparams[vector.position.in.listPparams]$cool.common)
  vector.name.in.listPparams <- names(listPparams)[vector.position.in.listPparams]
  matrix.name.in.listPparams <- names(listPparams)[ifelse(vector.position.in.listPparams == 1, 2, 1)]
  
  res <- panelPomp:::toListPparams(
    matrixPparams = matrixPparams,
    names.in.vector = names.in.vector,
    vector.position.in.listPparams = vector.position.in.listPparams,
    vector.name.in.listPparams = vector.name.in.listPparams,
    matrix.name.in.listPparams = matrix.name.in.listPparams
  )
  expect_true(object = identical(x = res, y = listPparams))
  
})

test_that("tolistPparams and toMatrixPparams work (both common & specific parameters)", {
  listPparams <-
    list(cool.common = common, yay.specific = specific)

  matrixPparams <- panelPomp:::toMatrixPparams(listPparams)
  vector.position.in.listPparams <- which(sapply(listPparams, is.vector))
  names.in.vector <- names(listPparams[vector.position.in.listPparams]$cool.common)
  vector.name.in.listPparams <- names(listPparams)[vector.position.in.listPparams]
  matrix.name.in.listPparams <- names(listPparams)[ifelse(vector.position.in.listPparams == 1, 2, 1)]
  
  res <- panelPomp:::toListPparams(
    matrixPparams = matrixPparams,
    names.in.vector = names.in.vector,
    vector.position.in.listPparams = vector.position.in.listPparams,
    vector.name.in.listPparams = vector.name.in.listPparams,
    matrix.name.in.listPparams = matrix.name.in.listPparams
  )
  expect_true(object = identical(x = res, y = listPparams))
})

test_that("runif.EstimationScale works", {
  res <- panelPomp:::runif.EstimationScale(centers = c(th = 1), widths = 2)
})
