library(panelPomp)
data(polio)

rproc <- Csnippet("
  double beta = exp(dot_product(K, &xi1, &b1));
  double lambda = (beta * (IO+IB) / P + psi);
  double var_epsilon = pow(sigma_dem,2)/lambda +  sigma_env*sigma_env;
  lambda *= (var_epsilon < 1.0e-6) ? 1 : rgamma(1/var_epsilon,var_epsilon);
  double p = exp(-(delta+lambda)/12);
  double q = (1-p)*lambda/(delta+lambda);
  SB1 = B;
  SB2 = SB1*p;
  SB3 = SB2*p;
  SB4 = SB3*p;
  SB5 = SB4*p;
  SB6 = SB5*p;
  SO = (SB6+SO)*p;
  IB = (SB1+SB2+SB3+SB4+SB5+SB6)*q;
  IO = SO*q;
")

dmeas <- Csnippet("
  double tol = 1.0e-25;
  double rho = 1/(1 + exp(rhoc + rhola*LAT + rholo*LONG));
  double mean_cases = rho*IO;
  double sd_cases = sqrt(pow(tau*IO,2) + mean_cases);
  if (cases > 0.0) {
    lik = pnorm(cases+0.5,mean_cases,sd_cases,1,0) - pnorm(cases-0.5,mean_cases,sd_cases,1,0) + tol; 
  } else{
    lik = pnorm(cases+0.5,mean_cases,sd_cases,1,0) + tol;
  }
  if (give_log) lik = log(lik);
")

rmeas <- Csnippet("
  double rho = 1/(1 + exp(rhoc + rhola*LAT + rholo*LONG));
  cases = rnorm(rho*IO, sqrt( pow(tau*IO,2) + rho*IO ) );
  if (cases > 0.0) {
    cases = nearbyint(cases);
  } else {
    cases = 0.0;
  }
")

init <- Csnippet("
  SB1 = SB1_0;
  SB2 = SB2_0;
  SB3 = SB3_0;
  SB4 = SB4_0;
  SB5 = SB5_0;
  SB6 = SB6_0;
  IB = 0;
  IO = IO_0 * P;
  SO = SO_0 * P;
")

toEst <- Csnippet("
 Tpsi = log(psi);
 Ttau = log(tau);
 Tsigma_dem = log(sigma_dem);
 Tsigma_env = log(sigma_env);
 TSO_0 =  logit(SO_0);
 TIO_0 = logit(IO_0);
")

fromEst <- Csnippet("
 Tpsi = exp(psi);
 Ttau = exp(tau);
 Tsigma_dem = exp(sigma_dem);
 Tsigma_env = exp(sigma_env);
 TSO_0 =  expit(SO_0);
 TIO_0 = expit(IO_0);
")

cdir <- if (exists("cdir",inherits=FALSE)) cdir else NULL
## Construct panelPomp object
unm <- levels(polioUS$state)
poList <- setNames(vector(mode="list",length=length(unm)),nm=unm)
paramsh <- c(rhoc=4.6,rhola=1e-3,rholo=1e-3,delta=1/60)
paramsp <- c(b1=3,b2=0,b3=1.5,b4=6,b5=5,b6=3,psi=0.002,tau=0.001,
             sigma_dem=0.04,sigma_env=0.5,SO_0=0.12,IO_0=0.001)
initial_births <- matrix(
  NA,
  nr=length(sbnms <- c("SB1_0","SB2_0","SB3_0","SB4_0","SB5_0","SB6_0")),
  nc=length(unm),
  dimnames=list(sbnms,unm)
)

for (i.u in unm) {
  # i.u <- "Texas"#"South Dakota"#levels(polio_data$state)[1]#
  
  # t0 is (end of) April 1932 except for Texas (1934) and South Dakota (1933)
  t0_u <- 1+(4/12) + floor(
    head(polioUS$time[polioUS$state == i.u&polioUS$births != 0],1))
  
  # The 'time' of 'data_u' is longer than that of the 'pomp' data argument
  # because 'data_u' has the birth data from before t=1
  data_u <- data.frame(subset(polioUS,(time>=(floor(t0_u)-1))&(state==i.u)))
  
  bspline_basis_u <- periodic.bspline.basis(
    data_u$time,nbasis=6,degree=3,period=1,names="xi%d"
  )
  smo_spl_x_u <- (floor(t0_u)-1):1954
  long_u <- rep(
    subset(longlat,subset=(NAME==i.u),select="long",drop=TRUE),
    length.out=length(data_u$time)
  )
  lat_u <- rep(
    subset(longlat,subset=(NAME==i.u),select="lat",drop=TRUE),
    length.out=length(data_u$time)
  )
  # Trial and error for 'rhoc', 'rhola', and 'rholo' (aim at about 0.02)
#st=unm[40];rhoc=4.6;rhola=1e-3;rholo=1e-3;1/(1 + exp(rhoc + rhola*subset(longlat,subset=(NAME==st),select="lat",drop=TRUE) + rholo*subset(longlat,subset=(NAME==st),select="long",drop=TRUE)))
  covartable_u <- data.frame(
    time=data_u$time,
    B=data_u$births,
    P=predict(
      smooth.spline(
        x=smo_spl_x_u,
        y=data_u$pop[12*(1:length(smo_spl_x_u))]
        # For all states but Texas and South Dakota, this is 
        #x = 1931:1954,
        #y = unit_data$pop[12 * (1:24)])
      ),
      x=data_u$time
    )$y,
    bspline_basis_u,
    LONG=long_u,
    LAT=lat_u
  )
  ii_u <- which(abs(covartable_u$time-t0_u)<0.01)
  initial_births[,i.u] <- as.numeric(covartable_u$B[ii_u-0:5])
  
  if (i.u==unm[[1]]) {
    pomp::pomp(
      data=data.frame(
        subset(data_u,(time>t0_u + 0.01)&(time<1953+(1/12)+0.01),
               select=c("time","cases")
        )
      ),
      times="time",
      t0=t0_u,
      params=c(paramsh,paramsp,initial_births[,i.u]),
      rprocess=euler.sim(step.fun = rproc, delta.t=1/12),
      rmeasure=rmeas,
      dmeasure=dmeas,
      covar=covartable_u,
      tcovar="time",
      statenames=c("IB","IO","SB1","SB2","SB3","SB4","SB5","SB6","SO"),
      paramnames=c("b1","b2","b3","b4","b5","b6","psi","rhoc","rhola","rholo",
                   "sigma_dem","sigma_env","tau","IO_0","SO_0","delta",sbnms),
      initializer=init,
      toEstimationScale=toEst, 
      fromEstimationScale=fromEst,
      globals="int K = 6;",
      cdir=cdir
    ) -> polio -> poList[[i.u]]
  } else {
    poList[[i.u]] <- polio
    polio.u <- pomp::pomp(
      data=data.frame(
        subset(data_u,(time>t0_u + 0.01)&(time<1953+(1/12)+0.01),
               select=c("time","cases")
        )
      ),
      times="time",
      t0=t0_u,
      params=c(paramsh,paramsp,initial_births[,i.u]),
      covar=covartable_u,
      tcovar="time"
    )
    poList[[i.u]]@data <- polio.u@data
    poList[[i.u]]@times <- polio.u@times
    poList[[i.u]]@t0 <- polio.u@t0
    poList[[i.u]]@covar <- polio.u@covar
    poList[[i.u]]@tcovar <- polio.u@tcovar
  }
}

panelPomp(
  poList,
  shared=paramsh,
  specific=rbind(
    matrix(paramsp,
           nrow=length(paramsp),
           ncol=length(poList),
           dimnames=list(names(paramsp),names(poList))),
    initial_births
  )
) -> panpolola

c("panpolola")
