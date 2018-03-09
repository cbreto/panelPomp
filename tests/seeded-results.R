# check reproducibility over a range of randomly chosen seeds

library(panelPomp,quietly=TRUE)

ppo <- pompExample(prw,envir=NULL)[[1]]

seeds <- c(21125715L,81902071L,86680005L,29971689L,73376915L)

for (s in seq_along(seeds)) {
  set.seed(seeds[s])
  pf <- pfilter(ppo,Np=10)
  print(unitlogLik(pf))
  print(logLik(pf))
}

for (s in seq_along(seeds)) {
  set.seed(seeds[s])
  mf <- mif2(ppo,Np=10,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),
             cooling.fraction.50=0.5,cooling.type="geometric")
  print(mf@pParams)
  print(unitlogLik(mf))
  print(logLik(mf))
  print(mf@pconv.rec)
  print(mf@pconv.rec.array)
}
