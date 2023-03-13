# check reproducibility over two randomly chosen seeds
if (file.exists("_options.R")) source("_options.R")
library(panelPomp,quietly=TRUE)

seeds <- c(21125715L,1888889L)
po <- panelPomp:::panelGompertz(N=5,U=2)[[1]]
for (s in seq_along(seeds)) {
  set.seed(seeds[s])
  print(obs(simulate(po)))
}

