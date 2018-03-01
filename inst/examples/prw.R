library(panelPomp)

# construct pomp object for random walk plus noise model
times <- 1:4
t0 <- 0
rproc <- Csnippet("X = X + rnorm(0,sigmaX);")
delta_t <- 1
rmeas <- Csnippet("Y = rnorm(X,sigmaY);")
dmeas <- Csnippet("lik = dnorm(Y,X,sigmaY,give_log);")
params <- c(sigmaY=1,sigmaX=1,X.0=1)
paramnm <- c("sigmaY","sigmaX")
statenm <- c("X")
pomp(data.frame(t=times,Y=NA),times="t",t0=t0,
     rprocess=discrete.time.sim(step.fun=rproc,delta.t=delta_t),rmeasure=rmeas,
     dmeasure=dmeas,params=params,paramnames=paramnm,statenames=statenm) -> rw

# construct panelPomp object with simulated data
# write & read simulation to facilitate robust reproducibility
U <- 2
#rws <- setNames(vector("list",length=U),nm=paste0("rw",1:U))
#set.seed(17375288L,kind="Mersenne-Twister",normal.kind="Inversion")
#packageVersion("pomp")
#[1] ‘1.16.1.1’
#for (u in seq_len(U)) rws[[u]] <- simulate(rw)
#rw_obs <- as.data.frame(sapply(rws,obs))
#write.csv2(rw_obs,row.names=FALSE)
rw_sim <- read.csv2(
  text = '"rw1";"rw2"
  2,08840863858374;1,68589578877002
  1,41921888369371;0,93744394535246
  3,40175540743253;4,731044919332
  2,23369156038638;1,91481177346194',
  header = TRUE
)
pos <- setNames(vector("list",length=U),nm=paste0("rw",1:U))
for (u in seq_len(U)) {
  pomp(data.frame(t=times,Y=as.numeric(rw_sim[,paste0("rw",u)])),times="t",
       t0=t0,rprocess=discrete.time.sim(step.fun=rproc,delta.t=delta_t),
       rmeasure=rmeas,dmeasure=dmeas,params=params,paramnames=paramnm,
       statenames=statenm) -> pos[[u]]
}
# check pomp objects in pos (see panelPomp/tests/prw.R)
panelPomp(pos,shared=coef(rw)[c("sigmaX","sigmaY") -> shnm],
          specific=matrix(coef(rw)[!names(coef(rw))%in%shnm] -> spparams,
                          nrow=length(spparams),ncol=U,
                          dimnames=list(param=names(spparams),
                                        unit=names(pos)))) -> prw
# check prw panelPomp object (see panelPomp/tests/prw.R)
c("prw")
