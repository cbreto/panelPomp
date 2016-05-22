#' @include pfilterd-ppomp-class.R
NULL

# pPfilter algorithm internal functions
pPfilter.internal <- function(object,
                              pParams,
                              Np,
                              ptol, 
                              ...) {
  # DEBUG
  # Turn params list into a matrix
  matrixpParams <- toMatrixPparams(pParams)
  U <- length(x = object)
  pfilterd.pomp.list <- as.vector(x = rep(NA, U), mode = "list")
  if(missing(ptol)){
    ptol <- structure(.Data = rep(1e-17, length(x = object)),
                     names = names(unitobjects(object = object))
    )
  }
  try(for (i.u in 1:U) {
    pfilterd.pomp.list[[i.u]] <-
      pomp::pfilter(object = object@unit.objects[[i.u]],
              params = matrixpParams[, i.u],
              Np = Np, 
              tol = unname(ptol[names(x = object@unit.objects[[i.u]])])
              )
  })
  names(pfilterd.pomp.list) <- names(x = object@unit.objects)
  pPfilter.internal.unit.logliks <-
    sapply(X = pfilterd.pomp.list, FUN = logLik)
  pPfilter.internal.loglik <-
    sum(pPfilter.internal.unit.logliks)
  return(
    new(
      Class = "pfilterd.ppomp",
      unit.objects = pfilterd.pomp.list,
      pParams = pParams,
      ploglik = pPfilter.internal.loglik,
      unit.logliks = pPfilter.internal.unit.logliks, 
      ptol = ptol
    )
  )
}