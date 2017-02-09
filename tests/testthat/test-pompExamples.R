library(pomp)
library(panelPomp)

context("Test 'pompExample(pangomp)'")

pang <- try(pompExample(pangomp,envir=NULL)[[1]])
if (class(pang)=="try-error") pang <- readRDS("pangomp.rds")

test_that("pompExample(pangomp) fails",{class(pang)=="panelPomp"})

test_that("pfilter fails on pangomp",
          {pang.pf <- pfilter(pang,Np=10)
          logLik(pang.pf)
          unitlogLik(pang.pf)
          }
)

test_that("mif2 fails on pangomp", 
          {pang.mif2 <- mif2(pang,
                           transform=TRUE,
                           Nmif=1,
                           rw.sd=rw.sd(r = 0.02),
                           Np=10,
                           cooling.type="geometric", 
                           cooling.fraction.50=0.5
          )
          pang.mif2@pconv.rec.array
          pang.mif2@pconv.rec  
          }
)

context("Test 'pompExample(pancon)'")

panc <- try(pompExample(pancon,envir=NULL)[[1]])
if (class(panc)=="try-error") panc <- readRDS("pancon.rds")

test_that("pompExample(pancon) fails",{class(panc)=="panelPomp"})

test_that("pfilter fails on pancon",
          {panc.pf <- pfilter(panc,Np=10)
          logLik(panc.pf)
          unitlogLik(panc.pf)
          }
)

test_that("mif2 fails on pancon",
          {panc.mif2 <- mif2(panc,
                           transform=TRUE,
                           Nmif=1,
                           rw.sd=rw.sd(mu_X=0.02),
                           Np=10,
                           cooling.type="geometric", 
                           cooling.fraction.50=0.5
          )
          panc.mif2@pconv.rec.array
          panc.mif2@pconv.rec  
          }
)


context("Test 'pompExample(panpol)'")

panp <- try(pompExample(panpol,envir=NULL)[[1]])
if (class(panp)=="try-error") panp <- readRDS("panpol.rds")

test_that("pompExample(panpol) fails",{class(panp)=="panelPomp"})

test_that("pfilter fails on panpol",
          {panp.pf <- pfilter(panp,Np=10)
          logLik(panp.pf)
          unitlogLik(panp.pf)
          }
)

test_that("mif2 fails on panpol",
          {panp.mif2 <- mif2(panp,
                           transform=TRUE,
                           Nmif=1,
                           rw.sd=rw.sd(rho=0.02),
                           Np=10,
                           cooling.type="geometric", 
                           cooling.fraction.50=0.5
          )
          panp.mif2@pconv.rec.array
          panp.mif2@pconv.rec  
          }
)

panpolola <- try(pompExample(panpolola,envir=NULL)[[1]])
if (class(panpolola)=="try-error") panpolola <- readRDS("panpolola.rds")

test_that("pompExample(panpolola) fails",{class(panpolola)=="panelPomp"})

test_that("pfilter fails on panpolola",
          {panpolola.pf <- pfilter(panpolola,Np=10)
          logLik(panpolola.pf)
          unitlogLik(panpolola.pf)
          }
)

test_that("mif2 fails on panpolola",
          {panpolola.mif2 <- mif2(panpolola,
                             transform=TRUE,
                             Nmif=1,
                             rw.sd=rw.sd(rhoc=0.02),
                             Np=10,
                             cooling.type="geometric", 
                             cooling.fraction.50=0.5
          )
          panpolola.mif2@pconv.rec.array
          panpolola.mif2@pconv.rec
          }
)
