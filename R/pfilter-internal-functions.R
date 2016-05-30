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
  U <- length(object)
  pfilterd.pomp.list <- setNames(vector(mode="list",length=U),names(unitobjects(object)))
  if (missing(ptol)) {
    ptol <- setNames(rep(1e-17,U),names(unitobjects(object)))
  }
  for (i.u in 1:U) {
    pfilterd.pomp.list[[i.u]] <-
      pomp::pfilter(
        object = object@unit.objects[[i.u]],
        params = matrixpParams[, i.u],
        Np = Np,
        tol = unname(ptol[i.u])
      )
  }
  pPfilter.internal.unit.logliks <- sapply(pfilterd.pomp.list,logLik)
  pPfilter.internal.loglik <- sum(pPfilter.internal.unit.logliks)
  new(
    Class = "pfilterd.ppomp",
    unit.objects = pfilterd.pomp.list,
    pParams = pParams,
    ploglik = pPfilter.internal.loglik,
    unit.logliks = pPfilter.internal.unit.logliks,
    ptol = ptol
  )
}
