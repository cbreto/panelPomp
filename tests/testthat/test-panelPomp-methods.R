library(panelPomp)

context("test-panelPomp-methods")

pompExample(panelGompertz)
ppo <- panelPomp(object = list(unit1 = gompertz, unit2 = gompertz))

test_that("mif2;panelPomp-method stop for missing start", {
  res <- try(mif2(object = ppo), silent = TRUE)
  expect_true(object = identical(x = class(res), y = "try-error"))
})

test_that("mif2;panelPomp-method stop for wrong shared.start names", {
  res <- try(mif2(object = panelGompertz, shared.start = c(sth = 0)), silent = TRUE)
  expect_true(object = identical(x = class(res), y = "try-error"))
})

test_that("mif2;panelPomp-method stop for wrong specific.start rownames", {
  sp.start <- coef(panelGompertz)$specific
  rownames(sp.start) <- c("some", "wrong", "names")
  res <- try(mif2(object = panelGompertz, specific.start = sp.start), silent = TRUE)
  expect_true(object = identical(x = class(res), y = "try-error"))
})

test_that("mif2;panelPomp-method stop for wrong specific.start colnames", {
  sp.start <- coef(panelGompertz)$specific
  colnames(sp.start) <- paste0(colnames(sp.start), "_")
  res <- try(mif2(object = panelGompertz, specific.start = sp.start), silent = TRUE)
  expect_true(object = identical(x = class(res), y = "try-error"))
})

test_that("mif2;panelPomp-method stops for missing Np", {
  res <- try(mif2(object = panelGompertz), silent = TRUE)
  expect_true(object = identical(x = class(res), y = "try-error"))
})

test_that("mif2;panelPomp-method stops for missing cooling.fraction.50", {
  res <- try(mif2(object = panelGompertz, Np = 10), silent = TRUE)
  expect_true(object = identical(x = class(res), y = "try-error"))
})

test_that("mif2;panelPomp-method stops for missing rw.sd", {
  res <- try(mif2(object = panelGompertz, Np = 10, cooling.fraction.50 = .5), silent = TRUE)
  expect_true(object = identical(x = class(res), y = "try-error"))
})

test_that("unitobjects returns specified unit", {
  res <- try(unitobjects(object = panelGompertz, unit = "unit1"), silent = TRUE)
  expect_true(object = identical(x = is(res), y = "pomp"))
})
