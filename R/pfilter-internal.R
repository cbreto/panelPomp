#' @include pfilterd-ppomp-class.R
NULL

# pPfilter algorithm internal functions
pPfilter.internal <- function(object,
                              params,
                              Np,
                              tol = 1e-17,
                              ...) {
  # DEBUG
  # Turn params list into a matrix
  matrixpParams <- toMatrixPparams(params)
  U <- length(object)
  pfilterd.pomp.list <- setNames(vector(mode="list",length=U),names(unitobjects(object)))
  if (length(tol)==1) {
    tol <- setNames(rep(tol,U),names(unitobjects(object)))
  } else if (length(tol) != U) {
    stop("in ",sQuote("pfilter"),": ",sQuote("tol"),
      " must be a single positive scalar or a vector of length ",
      U,call.=FALSE)
  }
  for (i.u in 1:U) {
    pfilterd.pomp.list[[i.u]] <-
      pomp::pfilter(
        object = object@unit.objects[[i.u]],
        params = matrixpParams[, i.u],
        Np = Np,
        tol = unname(tol[i.u]),
        ...
      )
  }
  pPfilter.internal.unit.logliks <- sapply(pfilterd.pomp.list,logLik)
  pPfilter.internal.loglik <- sum(pPfilter.internal.unit.logliks)
  new(
    Class = "pfilterd.ppomp",
    unit.objects = pfilterd.pomp.list,
    pParams = params,
    ploglik = pPfilter.internal.loglik,
    unit.logliks = pPfilter.internal.unit.logliks,
    ptol = tol
  )
}
