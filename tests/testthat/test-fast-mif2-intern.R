library(panelPomp)

context("Test panelPomp:::mif2.internal()")

pg <- try(pompExample(pangomp,envir=NULL)[[1]])
if (class(pg)=="try-error") pg <- readRDS("pangomp.rds")

gompertz <- pompExample(gompertz,envir=NULL)[[1]]
shgomp <- gompertz
time(shgomp) <- time(gompertz)[1:2]
shgomp@data <- gompertz@data[, 1:length(time(shgomp)),drop = FALSE]
short.pgompertz <- panelPomp(list(u1=shgomp,u2=shgomp),shared=coef(gompertz))
pPomp.object <- short.pgompertz
test_pmif2_internal <-
  panelPomp:::mif2.internal(
    pPomp.object,
    Nmif = 2,
    start = list(
      shared = c(
        #K = 1.0,
        r = 0.1,
        sigma = 0.1,
        tau = 0.1
      ),
      specific = array(
        #data = 1,
        data = c(1, 1),
        #dim = c(1, length(pPomp.object)),
        dim = c(2, length(pPomp.object)),
        #dimnames = list(c("X.0"), names(unitobjects(pPomp.object)))
        dimnames = list(c("X.0", "K"), names(unitobjects(pPomp.object)))
      )
    ),
    Np = 50,
    rw.sd = rw.sd(tau = 0.02, X.0 = ivp(0.2)),
    transform = TRUE,
    cooling.type = "geometric",
    cooling.fraction.50 = 0.5
  )

test_that("mif2.internal does not choke when given only one specific parameter", {
  mif2d.ppomp.with.only.one.specific.parameter <- panelPomp:::mif2.internal(
    pPomp.object,
    Nmif = 2,
    start = list(
      shared = c(
        K = 1.0,
        r = 0.1,
        sigma = 0.1,
        tau = 0.1
      ),
      specific = array(
        data = 1,
        #data = c(1, 1),
        dim = c(1, length(pPomp.object)),
        #dim = c(2, length(pPomp.object)),
        dimnames = list(c("X.0"), names(unitobjects(pPomp.object)))
        #dimnames = list(c("X.0", "K"), names(unitobjects(pPomp.object)))
      )
    ),
    Np = 50,
    rw.sd = rw.sd(tau = 0.02, X.0 = ivp(0.2)),
    transform = TRUE,
    cooling.type = "geometric",
    cooling.fraction.50 = 0.5
  )
  expect_true(
    object = 
      class(x = mif2d.ppomp.with.only.one.specific.parameter)=="mif2d.ppomp"
  )
})


test_that("mif2.internal does not choke when given both specific and shared parameters", {
  mif2d.ppomp.with.only.one.specific.parameter <- panelPomp:::mif2.internal(
    pPomp.object,
    Nmif = 2,
    start = list(
      shared = c(
        r = 0.1,
        sigma = 0.1,
        tau = 0.1
      ),
      specific = array(
        data = c(1, 1),
        dim = c(2, length(pPomp.object)),
        dimnames = list(c("X.0", "K"), names(unitobjects(pPomp.object)))
      )
    ),
    Np = 50,
    rw.sd = rw.sd(tau = 0.02, X.0 = ivp(0.2)),
    transform = TRUE,
    cooling.type = "geometric",
    cooling.fraction.50 = 0.5
  )
  expect_true(
    object = 
      class(x = mif2d.ppomp.with.only.one.specific.parameter)=="mif2d.ppomp"
  )
})


test_that("mif2.internal does not choke when given only a shared parameter", {
  mif2d.ppomp.with.only.one.specific.parameter <- panelPomp:::mif2.internal(
    pPomp.object,
    Nmif = 2,
    start = list(
      shared = c(tau = 0.1),
      specific = array(
        data = c(1, 1, 0.1, 0.1),
        dim = c(4, length(pPomp.object)),
        dimnames = list(c("X.0", "K", "r", "sigma"), names(unitobjects(pPomp.object)))
      )
    ),
    Np = 50,
    rw.sd = rw.sd(tau = 0.02, X.0 = ivp(0.2)),
    transform = TRUE,
    cooling.type = "geometric",
    cooling.fraction.50 = 0.5
  )
  expect_true(class(mif2d.ppomp.with.only.one.specific.parameter)=="mif2d.ppomp")
})

#test_that("Unit 2's paramMatrix matches unit 1's filtering distribution", {
#  # When "functionality X is being implemented as intended," then one should ...
#  expect_true(
#    object = all(test_pmif2_internal@pconv.rec$pparamArray.record[c("tau"),1:10,1,2]==
#      test_pmif2_internal@pconv.rec$pparamArray.record[c("tau"),1:10,2,2])
#    )
#  })

test_that("mif2d.ppomps can be mif2d again",
          {
            test.mif2ing.a.mif2d.ppomp <- mif2(object = test_pmif2_internal)
            expect_true(object =
                          class(x = test.mif2ing.a.mif2d.ppomp) == "mif2d.ppomp")
          })

test_that("mif2.internal pomp::mif2::tryCatch works",
          {
            res <- try(mif2(pg, Nmif = 2, Np = 50, 
                            rw.sd = rw.sd(something = 0.02, random = ivp(0.2)),
                            transform = TRUE, cooling.type = "geometric", 
                            cooling.fraction.50 = 0.5), silent = TRUE)
            expect_true(object = class(x = res) == "try-error")
          })
