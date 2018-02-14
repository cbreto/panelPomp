library(panelPomp)

ppo <- pompExample(prw,envir=NULL)[[1]]
pos <- as(ppo,"list")
po <- pos[[1]]

# check reproducibility over a range of randomly chosen seeds
seeds <- c(21125715L,81902071L,86680005L,29971689L,73376915L)
for (s in seq_along(seeds)) {
set.seed(seeds[s])
pf <- pfilter(ppo,Np=10)
print(unitlogLik(pf))
print(logLik(pf))
}

error_req_obj <- paste0(
  "Error : in ",sQuote("pfilter"),": ",sQuote("object")," is a required argument\n"
)
try(pfilter(sh=coef(ppo)$sh,sp=coef(ppo)$sp,Np=10),
    silent=TRUE)[1]==error_req_obj
try(pfilter(params=coef(ppo),Np=10),
    silent=TRUE)[1]==error_req_obj
