library(panelPomp)

vals <- numeric(60)

pompExample(panelGompertz)
  
for (i in seq_along(vals)) {
  
  set.seed(334388458L)
  
  pfilter(panelGompertz,Np = 100) -> m1
  vals[i] <- logLik(m1)
}

print(as.data.frame(table(vals)))
