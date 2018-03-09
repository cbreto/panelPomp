# pckg_src/tests/this_file.R
# Using pomp objects created with Csnipets in tests ran by package testthat 
# seems to be tricky. This file implements a work-around: create any such 
# object before 'R CMD check' runs 'testthat.R' and save them in the 'testthat' 
# folder.

# Include the following (changing the example name) in each file in the 
# 'testthat' folder that needs to use a panelPomp example:
## pg <- try(pompExample(pangomp,envir=NULL)[[1]])
## if (class(pg)=="try-error") pg <- readRDS("pangomp.rds")

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
