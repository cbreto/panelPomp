library(panelPomp)

context('Test logLik("matrix")')
test_that(
  "logLik, matrix-method fails to pass basic check (i.e., with duplicate logliks)",
  {
    repMargin <- 1
    unit1.ll <- 1
    unit2.ll <- 10
    object <- ulls <- matrix(c(unit1.ll,unit1.ll,unit2.ll,unit2.ll),nr=2,nc=2,
                   byrow=ifelse(repMargin==1,FALSE,TRUE))
    exp.ll <- unit1.ll + unit2.ll
    exp.se <- 0
    
    chck.avl <- logLik(ulls,repMargin=repMargin,first="aver",aver="logmeanexp",se=TRUE)
    chck.avm <- logLik(ulls,repMargin=repMargin,first="aver",aver="mean",se=TRUE)
    chck.agl <- logLik(ulls,repMargin=repMargin,first="aggr",aver="logmeanexp",se=TRUE)
    chck.agm <- logLik(ulls,repMargin=repMargin,first="aggr",aver="mean",se=TRUE)
    
    expect_true(all(c(
      chck.avl[1]==exp.ll,
      chck.avl[2]==exp.se,
      chck.avm[1]==exp.ll,
      chck.avm[2]==exp.se,
      chck.agl[1]==exp.ll,
      chck.agl[2]==exp.se,
      chck.agm[1]==exp.ll,
      chck.agm[2]==exp.se
    )))
  }
)

test_that(
  "logLik, matrix-method fails to pass check for first='aver' & aver='logmeanexp')",
  {
    repMargin <- 2
    unit1.ll <- 2
    unit2.ll <- 10
    object <- ulls <- matrix(
      c(log(0.5*exp(unit1.ll)),log(1.5*exp(unit1.ll)),unit2.ll,unit2.ll),
      nr=2,nc=2,byrow=ifelse(repMargin==1,FALSE,TRUE)
    )
    chck <- logLik(ulls,repMargin=repMargin,first="aver",aver="logmeanexp",se=TRUE)
    expect_true(chck[1]==(unit1.ll + unit2.ll))
  }
)

test_that(
  "logLik, matrix-method fails to pass check for aver='mean')",
  {
    repMargin <- 1
    unit1.ll <- 1
    unit2.ll <- 10
    object <- ulls <- matrix(
      c(unit1.ll+0.5,unit1.ll-0.5,unit2.ll,unit2.ll),
      nr=2,nc=2,byrow=ifelse(repMargin==1,FALSE,TRUE)
    )
    chck <- logLik(ulls,repMargin=repMargin,first="aggr",aver="mean",se=TRUE)
    expect_true(chck[1]==(unit1.ll + unit2.ll))
  }
)
