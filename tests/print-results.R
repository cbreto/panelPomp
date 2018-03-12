library(panelPomp,quietly=TRUE)

## list examples (avoiding listing package installation folder for R CMD check)
capture.output(panelPompExample())[2]
ppo <- panelPompExample("prw")
ppo
lapply(as(ppo,"list"),obs)
