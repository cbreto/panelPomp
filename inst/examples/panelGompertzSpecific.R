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
  shared = numeric(0),
  specific = matrix(
    data = coef(gompertz) -> specific.params,
    nrow = length(specific.params),
    ncol = U,
    dimnames = list(names(specific.params),
                    names(pompList))
  )
) -> panelGompertzSpecific

c("panelGompertzSpecific")
