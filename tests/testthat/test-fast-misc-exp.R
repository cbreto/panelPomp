library(panelPomp)

context("Test panelPomp's miscellania exported functions")

pg <- try(pompExample(pangomp,envir=NULL)[[1]])
if (class(pg)=="try-error") pg <- readRDS("pangomp.rds")

shared <- c(shared.1=1,shared.2=2)
u <- 5 # at least 1
specific <- matrix(
  as.numeric(paste0(rep(seq(3,u+2),each=2),rep(c(".1",".2"),times=u))),
  nrow=2,
  dimnames=list(c("spec.1","spec.2"),c(paste0("unit.",1:u)))
)

test_that("to- and fromVectorPparams don't match (with shared parameters only)", {
  listPparams <-
    list(shared=shared,
         specific=array(numeric(0),
                            dim=c(0,dim(specific)[2]),
                            dimnames=list(NULL,dimnames(specific)[[2]])
         )
    )
  vectorpParams <- toVectorPparams(listPparams)
  res <- fromVectorPparams(vectorpParams)
  expect_true(identical(res,listPparams))
})

test_that("to- and fromVectorPparams don't match (with specific parameters only)", {
  listPparams <- list(shared=numeric(0),specific=specific)
  vectorpParams <- panelPomp::toVectorPparams(listPparams)
  res <- panelPomp::fromVectorPparams(vectorpParams)
  expect_true(identical(res,listPparams))
})

test_that("to- and fromVectorPparams don't match (with both shared & specific parameters)", {
  listPparams <- list(shared=shared,specific=specific)
  vectorpParams <- panelPomp::toVectorPparams(listPparams)
  res <- panelPomp::fromVectorPparams(vectorpParams)
  expect_true(identical(res,listPparams))
})
