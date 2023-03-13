if (file.exists("_options.R")) source("_options.R")
library(panelPomp,quietly=TRUE)
set.seed(3496L)

g <- panelGompertz(N=5,U=2)
s1 <- simulate(g)
print(obs(s1[[1]]))
if(class(s1)!="panelPomp") stop("a single simulation should be a panelPomp")

s2 <- simulate(g,nsim=3)
if(length(s2)!=3) stop("if nsim>1, simulation returns a list of length nsim")

s3 <- simulate(g,specific=2*g@specific)
s4 <- simulate(g,shared=2*g@shared)
s5 <- simulate(g,specific=2*g@specific,shared=2*g@shared)

try(simulate(g,nsim="inappropriate text argument"))

g2 <- panelPomp(g@unit.objects)
try(simulate(g2))



