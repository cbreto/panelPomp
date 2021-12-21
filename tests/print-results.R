library(panelPomp,quietly=TRUE)
if (file.exists("options.R")) source("options.R")

## list examples (avoiding listing package installation folder for R CMD check)
ppo <- panelRandomWalk(U=2,N=6)
ppo
lapply(as(ppo,"list"),obs)

