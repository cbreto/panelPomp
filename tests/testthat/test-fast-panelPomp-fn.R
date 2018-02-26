library(panelPomp)

pg <- try(pompExample(pangomp,envir=NULL)[[1]])
if (class(pg)=="try-error") pg <- readRDS("pangomp.rds")

context("Test panelPomp()")
test_that(
  "panelPomp() fails to detect list of 'non-pomp' objects",
  {expect_error(panelPomp(list(a=1)),regexp="panelPomp",fixed=T)}
)
test_that(
  "panelPomp() fails to resolve simultaneous parameter assignments",
  {
    expect_error(
      panelPomp(
        as(pg,"list"),
        shared=2*pg@pParams$shared,
        specific=2*pg@pParams$specific,
        params=pg@pParams),
      regexp="panelPomp",
      fixed=T
    )
  }
)
test_that(
  "panelPomp() fails to resolve simultaneous assignments to 'shared'",
  {
    pp <- panelPomp(
      as(pg,"list"),
      shared=2*pg@pParams$shared,
      params=pg@pParams)
    expect_identical(pp@pParams$shared,2*pg@pParams$shared)
  }
)
test_that(
  "panelPomp() fails to resolve simultaneous assignments to 'specific'",
  {
    pp <- panelPomp(
      as(pg,"list"),
      specific=2*pg@pParams$specific,
      params=pg@pParams)
    expect_identical(pp@pParams$specific,2*pg@pParams$specific)
  }
)


#context("Test panelPomp validity")
#test_that(
#  "panelPomp class validity check fails to detect desynchronized parameters",
#  {expect_error(panelPomp(as(pg,"list")),regexp="invalid class",fixed=T)}
#)
