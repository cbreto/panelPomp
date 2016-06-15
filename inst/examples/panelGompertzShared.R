require(panelPomp)

## Load gompertz pomp object
pomp::pompExample(example = gompertz)

## Initialize list of pomps
U <- 50
pompList <- setNames(object = vector(mode = "list", length = U),
                     nm = paste0("unit", 1:U))
freeze(seed = 1455280898L, kind = "Mersenne-Twister", {
  for (i.u in 1:U) {
    pompList[[i.u]] <-
      pomp::simulate(object = gompertz, seed = 12345678 + i.u)
  }
})

## Construct panelPomp
panelPomp(
  object = pompList,
  shared = coef(gompertz),
  specific = matrix(
    data = numeric(0),
    nrow = 0,
    ncol = U,
    dimnames = list(NULL,
                    names(pompList))
  )
) -> panelGompertzShared

c("panelGompertzShared")
