# Creating '.rds' files with the panelPomp examples inside folder 'testthat' 
# allows to define the examples with Csnipets and use them in tests inside 
# the 'testthat' folder

library(pomp)
library(panelPomp)

pomp::bake("testthat/pangomp.rds",{
  pangomp <- pomp::pompExample(
    pangomp,
    envir=NULL,
    cdir=paste0(getwd(),"/testthat"))[[1]]
})

pomp::bake("testthat/pancon.rds",{
  pangomp <- pomp::pompExample(
    pancon,
    envir=NULL,
    cdir=paste0(getwd(),"/testthat"))[[1]]
})

pomp::bake("testthat/panpol.rds",{
  pangomp <- pomp::pompExample(
    panpol,
    envir=NULL,
    cdir=paste0(getwd(),"/testthat"))[[1]]
})
