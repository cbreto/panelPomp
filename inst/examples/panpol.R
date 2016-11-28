library(panelPomp)
data("polio")


cdir <- if (exists("cdir",inherits=FALSE)) cdir else NULL

pomp::pomp(data=data.frame(t=1:4,y=NA),
           cdir=cdir
) -> pol

## Construct panelPomp object
U <- levels(polio_dataset[,"state"])
poList <- setNames(vector(mode="list",length=U),
                   nm=paste0("unit",1:U))
for (i.u in seq_len(U)) {
  poList[[i.u]] <- con
  poList[[i.u]]@data <- t(
    matrix(
      as.matrix(contacts[i.u,1:4]),
      dimnames=list(NULL,"y")
    )
  )
}
panelPomp(poList,shared=coef(pol),specific=) -> panpol
c("panpol")
