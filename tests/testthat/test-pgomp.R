library(panelPomp)

context("Test panelGomp")

panelGomp <- readRDS("panelGomp.rds")

test_that("pfilter runs on panelGomp", {
  panelGomp.pf <- pfilter(panelGomp, Np = 10)
  logLik(panelGomp.pf)
  unitlogLik(panelGomp.pf)
})

test_that("mif2 runs on panelGomp", {
  panelGomp.mif2 <- 
    mif2(
      panelGomp,
      transform = TRUE,
      Nmif = 1,
      rw.sd = rw.sd(r = 0.02),
      Np = 10,
      cooling.type = "geometric", 
      cooling.fraction.50 = 0.5
    )
  panelGomp.mif2@pconv.rec.array
  panelGomp.mif2@pconv.rec  
})
