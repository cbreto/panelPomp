# check reproducibility over two randomly chosen seeds
if (file.exists("_options.R")) source("_options.R")
library(panelPomp,quietly=TRUE)

seeds <- c(2112533L,188933L)
po <- panelPomp:::panelRandomWalk(N=5,U=2)[[1]]
for (s in seq_along(seeds)) {
  set.seed(seeds[s])
  print(obs(simulate(po)))
}

