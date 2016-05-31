require(panelPomp)

## Load gompertz pomp object
pomp::pompExample(example = gompertz)

## Initialize list of pomps
U <- 50
pompList <- setNames(object = as.vector(1:U, mode = "list"),
                     nm = paste0("unit", 1:U))
freeze(seed=1455280898L,kind="Mersenne-Twister",{
  for(i.u in 1:U){
    pompList[[i.u]] <- pomp::simulate(object = gompertz, seed = 12345678 + i.u)
  }
})

## Construct panelPomp
panelPomp(
  object = pompList,
  shared = coef(gompertz)[names(coef(gompertz)) %in% c("r", "sigma", "tau")] -> shared.params,
  specific = matrix(
    data = coef(gompertz)[!names(coef(gompertz)) %in% names(shared.params)] -> specific.params,
    nrow = length(specific.params),
    ncol = U,
    dimnames = list(names(specific.params),
                    names(pompList))
  )
) -> panelGompertz

c("panelGompertz")
