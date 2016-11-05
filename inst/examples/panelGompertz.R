require(panelPomp)
pomp::pompExample(example=gompertz)
# change 'times' slot so that t0 is one time step before t1
gompertz <- window(gompertz,start=1,end=100)

U <- 50
ppoList <- setNames(vector("list",length=U),
                    nm=paste0("unit",1:U)
)
for (i.u in seq_len(U)) {
  ppoList[[i.u]] <- pomp::simulate(gompertz,seed=12345678+i.u)
}

panelPomp(ppoList,
          shared=coef(gompertz)[c("r", "sigma") -> shnm],
          specific=matrix(
            data=coef(gompertz)[!names(coef(gompertz)) %in% shnm] -> spparams,
            nrow=length(spparams),
            ncol=U,
            dimnames=list(names(spparams),
                          names(ppoList))
          )
) -> panelGompertz

c("panelGompertz")
