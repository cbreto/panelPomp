library(panelPomp)
pg <- pompExample(panelGompertz,envir=NULL)[[1]]

context("Test panelPomp::panelPomp()")
test_that(
  "panelPomp() fails to detect list of 'non-pomp' objects",
  {expect_error(panelPomp(list(a=1)),regexp="panelPomp::panelPomp",fixed=T)}
)
test_that(
  "panelPomp() fails to resolve simultaneous parameter assignments",
  {
    expect_error(
      panelPomp(
        as(pg,"list"),
        shared=2*coef(pg)$shared,
        specific=2*coef(pg)$specific,
        params=coef(pg)),
      regexp="panelPomp::panelPomp",
      fixed=T
    )
  }
)
test_that(
  "panelPomp() fails to resolve simultaneous assignments to 'shared'",
  {
    pp <- panelPomp(
      as(pg,"list"),
      shared=2*coef(pg)$shared,
      params=coef(pg))
    expect_identical(coef(pp)$shared,2*coef(pg)$shared)
  }
)
test_that(
  "panelPomp() fails to resolve simultaneous assignments to 'specific'",
  {
    pp <- panelPomp(
      as(pg,"list"),
      specific=2*coef(pg)$specific,
      params=coef(pg))
    expect_identical(coef(pp)$specific,2*coef(pg)$specific)
  }
)


#context("Test panelPomp validity")
#test_that(
#  "panelPomp class validity check fails to detect desynchronized parameters",
#  {expect_error(panelPomp(as(pg,"list")),regexp="invalid class",fixed=T)}
#)
