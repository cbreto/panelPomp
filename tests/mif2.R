library(panelPomp)

ppo <- pompExample(prw,envir=NULL)[[1]]
pos <- as(ppo,"list")
po <- pos[[1]]

# check reproducibility over a range of randomly chosen seeds
seeds <- c(21125715L,81902071L,86680005L,29971689L,73376915L)
for (s in seq_along(seeds)) {
set.seed(seeds[s])
mf <- mif2(ppo,Np=10,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),cooling.fraction.50=0.5)
print(coef(mf))
print(unitlogLik(mf))
print(logLik(mf))
}

try(mif2(Np=10,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),cooling.fraction.50=0.5),
    silent=TRUE)[1]==paste0(
      "Error : in ",sQuote("mif2"),": ",sQuote("object")," is a required argument\n")
try(mif2(ppo,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5),cooling.fraction.50=0.5),
    silent=TRUE)[1]=="Error in .local(object, ...) : Missing 'Np' argument.\n"
try(mif2(ppo,Np=10,cooling.fraction.50=0.5),
    silent=TRUE)[1]==paste0(
      "Error : ",sQuote("panelPomp::mif2")," error: missing ",sQuote("rw.sd")," argument.\n")
try(mif2(ppo,Np=10,rw.sd=rw.sd(sigmaX=0.05,X.0=0.5)),
    silent=TRUE)[1]=="Error in .local(object, ...) : Missing 'cooling.fraction.50' argument.\n"
