library(panelPomp)

simUnivpar <- read.table(
  text = '"unit1"	"unit2"
1.02968440881872	1.12680981316627
  1.04070053488929	0.833048139677005
  0.895136258827424	1.29828095465191
  0.863819633918966	1.1909971425079
  1.15312658319484	0.97410297816982
  0.985284028762155	0.774051590871001
  1.02513835490766	1.11473281645368
  0.963983712191803	0.97187396703635
  1.11588975734665	1.35775458761634
  1.16618463467135	1.24624117297014
  1.47452592258906	1.3346047170652
  1.2932372410488	1.38316904692634
  1.24436002800946	1.39514013758941
  0.937900248770046	1.07416989077693
  0.924636095015482	1.21046729907771
  1.00773992094283	0.953546906819108
  1.00348470310244	0.928783119217019
  1.13573125437821	1.00264942626878
  1.36705158630151	1.13819085055491
  1.375856448618	1.20476370705038',
  header = TRUE
)

## Construct gompertz pomp object
dmeas <- Csnippet("lik = dlnorm(Y,log(X),0.1,give_log);")
rmeas <- Csnippet("Y = rlnorm(log(X),0.1);") 
rproc <- Csnippet("
 double S = exp(-0.1*dt);
 double eps = rlnorm(0,0.1);
 X = pow(1,(1-S))*pow(X,S)*eps;")

# check for existing 'cdir' (to make 'univparthat' package work)
cdir <- if (exists("cdir",inherits=FALSE)) cdir else NULL

pomp::pomp(data=data.frame(t=1:20,Y=NA),
           times="t",
           t0=0,
           rprocess=discrete.time.sim(step.fun=rproc,delta.t=1),
           rmeasure=rmeas,
           dmeasure=dmeas,
           params=c(X.0=1),
           statenames=c("X"),
           fromEstimationScale=function (params,...) exp(params),
           toEstimationScale=function (params,...) log(params),
           cdir=cdir
) -> univpar

## Initialize list of pomps
U <- 2

poList <- setNames(vector(mode="list",length=U),
                   nm=paste0("unit",1:U))
for (i.u in seq_len(U)) {
  univpar_u <- pomp::pomp(
    data.frame(t=time(univpar),Y=as.numeric(simUnivpar[,i.u])),
    times="t",
    t0=timezero(univpar)
  )
  poList[[i.u]] <- univpar
  poList[[i.u]]@data <- univpar_u@data
}
## Construct panelPomp
panelPomp(poList,
          shared=coef(univpar)[c("X.0") -> shnm],
          specific=matrix(coef(univpar)[!names(coef(univpar))%in%shnm] -> spparams,
            nrow=length(spparams),
            ncol=U,
            dimnames=list(names(spparams),names(poList))
          )
) -> panunivpar

c("panunivpar")
