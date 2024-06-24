# set parameters in list form
prw <- panelRandomWalk(U = 4)
new_pars <- matrix(c(1, 1, 3, 2), nrow = 1)
dimnames(new_pars) <- list(param = "X.0", unit = c("rw1", "rw2", "rw3", "rw4"))
specific(prw) <- new_pars
