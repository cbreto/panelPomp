if (file.exists("_options.R")) source("_options.R")
library(panelPomp,quietly=TRUE)

## list examples (avoiding listing package installation folder for R CMD check)
ppo <- panelRandomWalk(U=2,N=6)
ppo
lapply(as(ppo,"list"),obs)

