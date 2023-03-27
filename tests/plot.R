if (file.exists("_options.R")) source("_options.R")
library(panelPomp,quietly=TRUE)

old_o <- options(digits=3)
png(file.path(tempdir(),"plot-%02d.png"),res=100)
ppo <- panelPomp:::panelRandomWalk(N=5,U=2)
plot(ppo)
dev.off()
options(old_o)
