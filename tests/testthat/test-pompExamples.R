library(pomp)
library(panelPomp)

context("Test 'pompExample(pangomp)'")

pg <- try(pompExample(pangomp,envir=NULL)[[1]])
if (class(pg)=="try-error") pg <- readRDS("pangomp.rds")

test_that("pompExample(pangomp) fails",{class(pg)=="panelPomp"})

test_that("pfilter runs on pangomp",
          {pg.pf <- pfilter(pg, Np = 10)
          logLik(pg.pf)
          unitlogLik(pg.pf)
          }
)

test_that("mif2 runs on pangomp", 
          {pg.mif2 <- mif2(pg,
                           transform=TRUE,
                           Nmif=1,
                           rw.sd=rw.sd(r = 0.02),
                           Np=10,
                           cooling.type="geometric", 
                           cooling.fraction.50=0.5
          )
          pg.mif2@pconv.rec.array
          pg.mif2@pconv.rec  
          }
)

context("Test 'pompExample(pancon)'")

pc <- try(pompExample(pancon,envir=NULL)[[1]])
if (class(pc)=="try-error") pc <- readRDS("pancon.rds")

test_that("pompExample(pancon) fails",{class(pc)=="panelPomp"})

test_that("pfilter runs on pancon",
          {pc.pf <- pfilter(pc, Np = 10)
          logLik(pc.pf)
          unitlogLik(pc.pf)
          }
)

test_that("mif2 runs on pancon",
          {pc.mif2 <- mif2(pc,
                           transform=TRUE,
                           Nmif=1,
                           rw.sd=rw.sd(mu_X=0.02),
                           Np=10,
                           cooling.type="geometric", 
                           cooling.fraction.50=0.5
          )
          pc.mif2@pconv.rec.array
          pc.mif2@pconv.rec  
          }
)
