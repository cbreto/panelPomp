library(panelPomp)

## Construct gompertz pomp object
dmeas <- Csnippet("lik = dlnorm(Y,log(X),tau,give_log);")
rmeas <- Csnippet("Y = rlnorm(log(X),tau);") 
rproc <- Csnippet("
 double S = exp(-r*dt);
 double eps = (sigma > 0.0) ? rlnorm(0,sigma) : 1.0;
 X = pow(K,(1-S))*pow(X,S)*eps;")

cdir <- if (exists("cdir",inherits=FALSE)) cdir else NULL

pomp::pomp(data=data.frame(t=1:100,Y=NA),
           times="t",
           t0=0,
           rprocess=discrete.time.sim(step.fun=rproc,delta.t=1),
           rmeasure=rmeas,
           dmeasure=dmeas,
           params=c(K=1,r=0.1,sigma=0.1,tau=0.1,X.0=1),
           paramnames=c("K","r","sigma","tau"),
           statenames=c("X"),
           fromEstimationScale=function (params,...) exp(params),
           toEstimationScale=function (params,...) log(params),
           cdir=cdir
) -> gomp

## Initialize list of pomps
U <- 50
poList <- setNames(vector(mode="list",length=U),
                   nm=paste0("unit",1:U))
for (i.u in seq_len(U)) poList[[i.u]] <- pomp::simulate(gomp,seed=12345678+i.u)

## Construct panelPomp
panelPomp(poList,
          shared=coef(gomp)[c("r","sigma") -> shnm],
          specific=matrix(coef(gomp)[!names(coef(gomp))%in%shnm] -> spparams,
            nrow=length(spparams),
            ncol=U,
            dimnames=list(names(spparams),names(poList))
          )
) -> pangomp

c("pangomp")
