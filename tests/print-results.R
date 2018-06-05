library(panelPomp,quietly=TRUE)
if (file.exists("options.R")) source("options.R")

## list examples (avoiding listing package installation folder for R CMD check)
capture.output(panelPompExample())[2]
ppo <- panelPompExample("prw")
ppo
lapply(as(ppo,"list"),obs)
