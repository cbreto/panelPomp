library(panelPomp)
data("contacts")

dmeas <- Csnippet("lik = dnbinom(y, D, D/(D+C), give_log);")
rmeas <- Csnippet("y = rnbinom(D, D/(D+C));")

rproc <- Csnippet("double Zcum, tol=0.000001;
if( (int)t % 4 == 0) { 
D = (sigma_D < tol || mu_D < tol) ? mu_D : 
rgamma(pow(mu_D/sigma_D,2), pow(sigma_D,2)/mu_D);
if(D < tol) {D = tol;}
R = (sigma_R < tol || mu_R < tol) ? mu_R : 
rgamma(pow(mu_R/sigma_R, 2), pow(sigma_R, 2)/mu_R);
X = (sigma_X < tol || mu_X < tol) ? mu_X : 
rgamma(pow(mu_X/sigma_X, 2), pow(sigma_X, 2)/mu_X);
Z = (R < tol) ? 1/tol : rexp(1/R);
}
C = 0;
Zcum = Z;
while(Zcum < 6){
C  += Z * X;
Z = (R < tol) ? 1/tol : rexp(1/R);
X = (sigma_X < tol || mu_X < tol) ? mu_X : 
rgamma(pow(mu_X/sigma_X, 2), pow(sigma_X, 2)/mu_X);
Zcum += Z;
}
C += (6 - (Zcum - Z)) * X;
C *= pow(alpha, (int)t % 4 ); 
Z = Zcum - 6;")

toEst <- Csnippet("Tmu_X = log(mu_X);
  Tsigma_X = log(sigma_X);
  Tmu_D = log(mu_D);
  Tsigma_D = log(sigma_D);
  Tmu_R = log(mu_R);
  Talpha = log(alpha/(1-alpha));")

fromEst <- Csnippet("Tmu_X = exp(mu_X);
  Tsigma_X = exp(sigma_X);
  Tmu_D = exp(mu_D);
  Tsigma_D = exp(sigma_D);
  Tmu_R = exp(mu_R);
  Talpha = exp(alpha)/(1+exp(alpha));")

cdir <- if (exists("cdir",inherits=FALSE)) cdir else NULL

pomp::pomp(data=data.frame(t=1:4,y=NA),
           times="t",
           t0=0,
           rprocess=discrete.time.sim(step.fun=rproc,delta.t=1),
           rmeasure=rmeas,
           dmeasure=dmeas,
           params=c(mu_X=1.75,sigma_X=2.67,mu_D=3.81,sigma_D=4.42,mu_R=0.04,
                    sigma_R=0,alpha=0.90),
           paramnames=c("mu_X","sigma_X","mu_D","sigma_D","mu_R","sigma_R",
                        "alpha"),
           statenames=c("X","D","R","C","Z"),
           obsnames="y",
           fromEstimationScale=fromEst,
           toEstimationScale=toEst,
           initializer = function (params, t0, ...) c(X=0,D=0,R=0,C=0,Z=0),
           cdir=cdir
) -> con

## Initialize list of pomps
U <- 882
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


## Construct panelPomp
panelPomp(object=poList,
          shared=coef(con)
) -> pancon

c("pancon")
