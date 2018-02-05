# Creating '.rds' files with the panelPomp examples inside folder 'testthat' 
# allows to define the examples with Csnipets and use them in tests inside 
# the 'testthat' folder

# Include the following in each file that needs to use a panelPomp example
#pg <- try(pompExample(pangomp,envir=NULL)[[1]])
#if (class(pg)=="try-error") pg <- readRDS("pangomp.rds")

library(pomp)
library(panelPomp)

pomp::bake("testthat/pangomp.rds",{
  pomp::pompExample(
    pangomp,
    envir=NULL,
    cdir=paste0(getwd(),"/testthat"))[[1]]
})

pomp::bake("testthat/pancon.rds",{
  pomp::pompExample(
    pancon,
    envir=NULL,
    cdir=paste0(getwd(),"/testthat"))[[1]]
})

#pomp::bake("testthat/panpol.rds",{
#  pomp::pompExample(
#    panpol,
#    envir=NULL,
#    cdir=paste0(getwd(),"/testthat"))[[1]]
#})
