# check reproducibility over a range of randomly chosen seeds
library(panelPomp,quietly=TRUE)

seeds <- c(21125715L)#,81902071L,86680005L,29971689L,73376915L)

ppo <- pompExample(pangomp,envir=NULL)[[1]]

po <- as(ppo,"list")[[1]]
coef(po) <- c(ppo@shared,ppo@specific[,1])
po <- window(po,start=time(po)[1],end=time(po)[1])
lapply(as(ppo,"list"),obs)
for (s in seq_along(seeds)) {
  set.seed(seeds[s])
  print(obs(simulate(po)))
}

ppo <- pompExample(pancon,envir=NULL)[[1]]

po <- as(ppo,"list")[[1]]
coef(po) <- c(ppo@shared,ppo@specific[,1])
po <- window(po,start=time(po)[1],end=time(po)[1])
lapply(as(ppo,"list"),obs)
for (s in seq_along(seeds)) {
  set.seed(seeds[s])
  print(obs(simulate(po)))
}
