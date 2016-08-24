library(panelPomp)

pompExample(panelGompertz)

sigma <- 0.02
pmf <- mif2(
  panelGompertz,
  transform = TRUE,
  Nmif = 1,
  rw.sd = rw.sd(r = sigma),
  Np = 10,
  cooling.type = "geometric",
  cooling.fraction.50 = 0.5
)
stopifnot(class(pmf) == "mif2d.ppomp")

shnames <- 0.02
pmf <- mif2(
  panelGompertz,
  transform = TRUE,
  Nmif = 1,
  rw.sd = rw.sd(r = shnames),
  Np = 10,
  cooling.type = "geometric",
  cooling.fraction.50 = 0.5
)
stopifnot(class(pmf) == "mif2d.ppomp")
