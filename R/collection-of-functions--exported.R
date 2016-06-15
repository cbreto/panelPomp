#' @include collection-of-functions--not-exported.R
NULL

### panel_logmeanexp
#' Log-mean-exp for panels.
#'
#' Log-mean-exp for panels.
#'
#' When \code{se = TRUE}, the jackknife se's from \code{logmeanexp} are squared and summed.
#'
#' @param x Matrix with the same number of replicated estimates for each panel unit loglikelihood.
#' @param MARGIN The dimension of the matrix that corresponds to a panel unit and over which averaging occurs (1 indicates rows, 2 indicates columns).
#' @param se logical; whether to give standard errors
#' 
#' @export
#'
panel_logmeanexp <- function (x, MARGIN, se = FALSE) 
{
  # DEBUG
  #x <-
  #  array(
  #    data = c(log(1e-310), log(1e-220), log(1e-308), log(1e-300)),
  #    dim = c(2, 2),
  #    dimnames = list(unit = paste0("unit", 1:2), rep = NULL)
  #  ); x
  #MARGIN <- 1
  #se <- TRUE
  #panel_logmeanexp(x = x, MARGIN = MARGIN, se = se)
  #loglik.reps <- x[2,]; loglik.reps

  logmeanexps <- apply(X = x, MARGIN = MARGIN, FUN = logmeanexp, se = TRUE)
  if (se) {
    c(sum(logmeanexps[1,]),
      se = sum((unname(logmeanexps[2,]))^2)^(1/2))
  } else {
    logmeanexps
  }
}



### toVectorPparams
#' toVectorPparams.
#'
#' toVectorPparams.
#'
#' Convert from the format of the \code{pParams} slot of \code{panelPomp} objects to a one-row \code{data.frame}. This facilitates keeping a record of evaluated likeilhoods.
#'
#' @param listPparams A list with the format of the \code{pParams} slot of \code{panelPomp} objects.
#' 
#' @export
#'
toVectorPparams <- function(listPparams) {
  # BEGIN DEBUG
  #pompExample(panelGompertz)
  #pompExample(panelGompertzShared)
  #pompExample(panelGompertzSpecific)
  #listPparams <- listPparams.shared.only <- coef(panelGompertzShared)
  #listPparams <- listPparams.specific.only <- coef(panelGompertzSpecific)
  #listPparams <- coef(panelGompertz)
  # END DEBUG
  
  # rbind replicated shared parameters with matrix of specific parameters
  matrixPparams <- rbind(matrix(
    rep(x = listPparams$shared,
        times = ncol(listPparams$specific)),
    ncol = ncol(listPparams$specific),
    dimnames = list(names(listPparams$shared), NULL)
  ),
  listPparams$specific
  )
  # vectorize the matrix
  vectorPparams <- setNames(object = as.vector(matrixPparams),
                            nm = paste0(rep(
                              colnames(matrixPparams), each = nrow(matrixPparams)
                            ),
                            rownames(matrixPparams)))
  # Append info about nature of parameters (shared and specific)
  pParam.type <-
    setNames(object = c(rep("shared", times = length(listPparams$shared)), rep("specific", times = nrow(listPparams$specific))),
             nm = c(names(listPparams$shared), rownames(listPparams$specific)))
  # return
  merge(data.frame(t(pParam.type), stringsAsFactors = FALSE),
        data.frame(t(vectorPparams)))
}



### fromVectorPparams
#' fromVectorPparams.
#'
#' fromVectorPparams.
#'
#' Convert back to the format of the \code{pParams} slot \code{panelPomp} objects from a one-row \code{data.frame} (e.g., the row of a record of evaluated likelihoods with the maximum likelihood).
#'
#' @param vectorPparams A one-row \code{data.frame} with format matching that of the output of \link{toVectorPparams}.
#' @param unit.names Names of units.
#' @param names.of.shared.params Names of shared parameters.
#' @param names.of.specific.params Names of specific parameters.
#' 
#' @export
#'
fromVectorPparams <-
  function(vectorPparams,
           unit.names,
           names.of.shared.params,
           names.of.specific.params) {
    # BEGIN DEBUG
    #pompExample(panelGompertz)
    #pompExample(panelGompertzShared)
    #pompExample(panelGompertzSpecific)
    ##listPparams <- listPparams.shared.only <- coef(panelGompertzShared); vectorPparams <- toVectorPparams(listPparams); unit.names <- colnames(listPparams$specific)
    ##listPparams <- listPparams.specific.only <- coef(panelGompertzSpecific); vectorPparams <- toVectorPparams(listPparams); unit.names <- colnames(listPparams$specific)
    #listPparams <- coef(panelGompertz); vectorPparams <- toVectorPparams(listPparams); unit.names <- colnames(listPparams$specific); names.of.shared.params <- names(listPparams$shared); names.of.specific.params <- rownames(listPparams$specific)
    # END DEBUG
    
    # shared
    vector.of.shared.params <-
      setNames(object = as.numeric(vectorPparams[, paste0(unit.names[1], names.of.shared.params)]),
               nm = names.of.shared.params)
    # specific
    matrix.of.specific.params <- NULL
    for (i.u in 1:length(unit.names)) {
      matrix.of.specific.params <- cbind(matrix.of.specific.params,
                                         as.numeric(vectorPparams[, paste0(unit.names[i.u], names.of.specific.params)]))
    }
    dimnames(matrix.of.specific.params) <-
      list(names.of.specific.params, unit.names)
    # combine into list
    listpParams <- list(shared = vector.of.shared.params,
                        specific = matrix.of.specific.params)
    # return
    listpParams
  }