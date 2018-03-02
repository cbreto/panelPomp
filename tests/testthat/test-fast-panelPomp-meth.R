library(panelPomp)

pg <- try(pompExample(pangomp,envir=NULL)[[1]])
if (class(pg)=="try-error") pg <- readRDS("pangomp.rds")

g <- pompExample(gompertz,envir=NULL)[[1]]

pp <- panelPomp(
  list(unit1=g,unit2=g),
  shared=pg@pParams$shared,
  specific=pg@pParams$specific[,1:2]
)


context("Test as()")
test_that(
  "as(,'list') returns list of units",
  {
    res <- try(as(pg,"list"),silent=TRUE)
    expect_true(identical(res,pg@unit.objects))
  }
)


context("Test mif2()")
test_that(
  "mif2 stop fails for missing shared start & pParams slot",
  {
    res <- try(mif2(object = pp), silent = TRUE)
    expect_true(object = identical(x = class(res), y = "try-error"))
  }
)
test_that(
  "mif2 stop fails for missing specific start & missing pParams slot",
  {
    res <- try(mif2(object = pp, shared.start = pg@pParams$shared), silent = TRUE)
    expect_true(object = identical(x = class(res), y = "try-error"))
  }
)
test_that(
  "mif2 stop fails for wrong shared.start names",
  {
    res <- try(mif2(object = pg, shared.start = c(sth = 0)), silent = TRUE)
    expect_true(object = identical(x = class(res), y = "try-error"))
  }
)
test_that(
  "mif2 stop fails for wrong specific.start rownames",
  {
    sp.start <- pg@pParams$specific
    rownames(sp.start) <- c("some", "wrong", "names")
    res <- try(mif2(object = pg, specific.start = sp.start), silent = TRUE)
    expect_true(object = identical(x = class(res), y = "try-error"))
  }
)
test_that(
  "mif2 stop fails for wrong specific.start colnames",
  {
    sp.start <- pg@pParams$specific
    colnames(sp.start) <- paste0(colnames(sp.start), "_")
    res <- try(mif2(object = pg, specific.start = sp.start), silent = TRUE)
    expect_true(object = identical(x = class(res), y = "try-error"))
  }
)
test_that(
  "mif2 stop fails for missing Np",
  {
    res <- try(
      mif2(object = pg),
      silent = TRUE)
    expect_identical(object = is(res), expected = "try-error")
  }
)
test_that(
  "mif2 stop fails for missing cooling.fraction.50",
  {
    res <- try(
      mif2(object = pg, Np = 10),
      silent = TRUE)
    expect_identical(object = is(res), expected = "try-error")
  }
)
test_that(
  "mif2 stop fails for missing rw.sd",
  {
    res <- try(
      mif2(object = pg, Np = 10, cooling.fraction.50 = .5),
      silent = TRUE)
    expect_identical(object = is(res), expected = "try-error")
  }
)
test_that(
  "mif2() fails to replace parameters",
  {
    mfdpp <- mif2(
      pp,
      shared.start=2*pp@pParams$shared,
      specific.start=2*pp@pParams$specific,
      Np=10,
      rw.sd=rw.sd(r=0.2),
      cooling.fraction.50=0.5,
      cooling.type="geometric"
    )
    expect_identical(
      conv.rec(as(mfdpp,Class="list")[[1]])[1,-(1:2)],
      expected=c(2*pp@pParams$shared,2*pp@pParams$specific[,1])
    )
  }
)
test_that(
  "mif2() fails to resolve multiple 'shared' specifications",
  {
    mfdpp <- mif2(
      pp,
      shared.start=2*pp@pParams$shared,
      start=pp@pParams,
      Np=10,
      rw.sd=rw.sd(r=0.2),
      cooling.fraction.50=0.5,
      cooling.type="geometric"
    )
    expect_identical(
      conv.rec(as(mfdpp,Class="list")[[1]])[1,-(1:2)],
      expected=c(2*pp@pParams$shared,pp@pParams$specific[,1])
    )
  }
)
test_that(
  "mif2() fails to resolve multiple 'specific' specifications",
  {
    mfdpp <- mif2(
      pp,
      specific.start=2*pp@pParams$specific,
      start=pp@pParams,
      Np=10,
      rw.sd=rw.sd(r=0.2),
      cooling.fraction.50=0.5,
      cooling.type="geometric"
    )
    expect_identical(
      conv.rec(as(mfdpp,Class="list")[[1]])[1,-(1:2)],
      expected=c(pp@pParams$shared,2*pp@pParams$specific[,1])
    )
  }
)
test_that(
  "mif2() fails to resolve multiple specifications",
  {
    expect_error(
      mif2(
        pp,
        shared.start=2*pp@pParams$specific,      
        specific.start=2*pp@pParams$specific,
        start=pp@pParams,
        Np=10,
        rw.sd=rw.sd(r=0.2),
        cooling.fraction.50=0.5,
        cooling.type="geometric"
      ),
      regexp="mif2",
      fixed=T
    )
  }
)

context("Test pfilter()")
test_that(
  "pfilter stop fails for missing shared",
  {
    res <- try(
      pfilter(object = pp),
      silent = TRUE)
    expect_identical(object = is(res), expected = "try-error")
  }
)
test_that(
  "pfilter stop fails for missing specific",
  {
    res <- try(
      pfilter(object = pp, shared = pg@pParams$shared),
      silent = TRUE)
    expect_identical(object = is(res), expected = "try-error")
  }
)
test_that(
  "pfilter stop fails for wrong shared names",
  {
    res <- try(
      pfilter(object = pg, shared = c(sth = 0)),
      silent = TRUE)
    expect_identical(object = is(res), expected = "try-error")
  }
)
test_that(
  "pfilter stop fails for wrong specific rownames",
  {
    sp <- pg@pParams$specific
    rownames(sp) <- c("some", "wrong", "names")
    res <- try(
      pfilter(object = pg, specific = sp),
      silent = TRUE)
    expect_identical(object = is(res), expected = "try-error")
  }
)
test_that(
  "pfilter stop fails for wrong specific colnames",
  {
    sp <- pg@pParams$specific
    colnames(sp) <- paste0(colnames(sp), "_")
    res <- try(
      pfilter(object = pg, specific = sp),
      silent = TRUE)
    expect_identical(object = is(res), expected = "try-error")
  }
)
test_that(
  "pfilter stop fails for missing Np",
  {
    res <- try(pfilter(pg),silent=TRUE)
    expect_identical(is(res),expected="try-error")
  }
)
test_that(
  "pfilter() fails to replace parameters",
  {
    pfdpp <- pfilter(
      pp,
      shared=2*pp@pParams$shared,
      specific=2*pp@pParams$specific,
      Np=10
    )
    expect_identical(
      coef(as(pfdpp,Class="list")[[1]]),
      expected=c(2*pp@pParams$shared,2*pp@pParams$specific[,1])
    )
  }
)
test_that(
  "pfilter() fails to resolve multiple 'shared' specifications",
  {
    pfdpp <- pfilter(
      pp,
      shared=2*pp@pParams$shared,
      params=pp@pParams,
      Np=10
    )
    expect_identical(
      coef(as(pfdpp,Class="list")[[1]]),
      expected=c(2*pp@pParams$shared,pp@pParams$specific[,1])
    )
  }
)
test_that(
  "pfilter() fails to resolve multiple 'specific' specifications",
  {
    pfdpp <- pfilter(
      pp,
      specific=2*pp@pParams$specific,
      params=pp@pParams,
      Np=10
    )
    expect_identical(
      coef(as(pfdpp,Class="list")[[1]]),
      expected=c(pp@pParams$shared,2*pp@pParams$specific[,1])
    )
  }
)
test_that(
  "pfilter() fails to resolve multiple specifications",
  {
    expect_error(
      pfilter(
        pp,
        shared=2*pp@pParams$shared, 
        specific=2*pp@pParams$specific,
        params=pp@pParams,
        Np=10
      ),
      regexp="pfilter",
      fixed=T
    )
  }
)
