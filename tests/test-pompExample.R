# Creating '.rds' files with the panelPomp examples inside folder 'testthat' 
# allows for defining the examples with Csnipets and use them in tests in 
# the 'testthat' folder

library(pomp)
library(panelPomp)

pomp::bake("testthat/panelGomp.rds",{
  panelGomp <- pomp::pompExample(
    panelGomp,
    envir=NULL,
    cdir=paste0(getwd(),"/testthat"))[[1]]
})
