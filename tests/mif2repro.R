library(panelPomp)

vals <- numeric(60)

pompExample(panelGompertz)
  
for (i in seq_along(vals)) {
  
  set.seed(334388458L)
  
  mif2(
    panelGompertz,
    shared = coef(panelGompertz)$shared,
    specific = coef(panelGompertz)$specific,
    transform = TRUE,
    Np = 100,
    prw.sd = substitute(pomp::rw.sd(r = 0.02)),
    Nmif = 1,
    cooling.type = "geometric",
    cooling.fraction.50 = 0.5
  ) -> m1
  vals[i] <- coef(m1)$shared["r"]
}

print(as.data.frame(table(vals)))
