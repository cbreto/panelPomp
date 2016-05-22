library(panelPomp)

context("Test pompExamples")

pomp::pompExample(panelGompertz)

test_that("pfilter runs on panelGompertz", {
  panelGompertz.pf <- pfilter(panelGompertz, Np = 10)
  logLik(panelGompertz.pf)
  unitlogLik(panelGompertz.pf)
})

test_that("mif2 runs on panelGompertz", {
  panelGompertz.mif2 <- 
    mif2(
      panelGompertz,
      shared.start= coef(unitobjects(panelGompertz)[[1]]),
      transform = TRUE,
      Nmif = 1,
      prw.sd = substitute(pomp::rw.sd(r = 0.02)),
      Np = 10,
      cooling.type = "geometric", 
      cooling.fraction.50 = 0.5
    )
  panelGompertz.mif2@pconv.rec.array
  panelGompertz.mif2@pconv.rec  
})