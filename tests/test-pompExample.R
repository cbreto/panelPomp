# Creating '.rds' files with the panelPomp examples inside folder 'testthat' 
# allows for defining the examples with Csnipets and use them in tests in 
# the 'testthat' folder

library(panelPomp)

pomp::bake("testthat/panelGomp.rds",{
  panelGomp <- pompExample(panelGomp,envir=NULL)[[1]]
})
