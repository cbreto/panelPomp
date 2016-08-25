#' @include misc-unexported.R
NULL

### panel_logmeanexp
#' Log-mean-exp for panels.
#'
#' Log-mean-exp for panels.
#'
#' When \code{se = TRUE}, the jackknife se's from \code{logmeanexp} are squared, summed and the squared root is taken.
#'
#' @param x Matrix with the same number of replicated estimates for each panel unit loglikelihood.
#' @param MARGIN The dimension of the matrix that corresponds to a panel unit and over which averaging occurs (1 indicates rows, 2 indicates columns).
#' @param se logical; whether to give standard errors
#' 
#' @export
#'
panel_logmeanexp <- function (x, MARGIN, se = FALSE)
{
  logmeanexps <-apply(X = x, MARGIN = MARGIN, FUN = logmeanexp, se = TRUE)
  summed <- sum(logmeanexps[1, ])
  if (!se) {
    summed
  } else {
    c(summed,
      se = sqrt(sum(unname(logmeanexps[2, ]) ^ 2))
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
  # Append info about ...
  #... nature of parameters (shared and specific), and ...
  pParam.type <-
    setNames(object = c(rep("shared", times = length(listPparams$shared)), rep("specific", times = nrow(listPparams$specific))),
             nm = c(names(listPparams$shared), rownames(listPparams$specific)))
  # ... unit names
  pre.unit.names <- deparse(colnames(listPparams$specific))
  unit.names <- NULL
  for(i in 1:length(pre.unit.names)){
    unit.names <- paste0(unit.names, pre.unit.names[i])
  }
  
  # return
  merge(x = data.frame(t(pParam.type), stringsAsFactors = FALSE),
        y = merge(x = data.frame(t(vectorPparams)),
                  y = data.frame(unit.names, stringsAsFactors = FALSE)
        )
  )
}



### fromVectorPparams
#' fromVectorPparams.
#'
#' fromVectorPparams.
#'
#' Convert back to the format of the \code{pParams} slot \code{panelPomp} objects from a one-row \code{data.frame} (e.g., the row of a record of evaluated likelihoods with the maximum likelihood).
#'
#' @param vectorPparams A one-row \code{data.frame} with format matching that of the output of \link{toVectorPparams}.
#' 
#' @export
#'
fromVectorPparams <- function(vectorPparams) {
    # Extract unit names (from last column) and shared and specific names
    unit.names <- eval(parse(text = vectorPparams[, ncol(vectorPparams)]))
    names.of.shared.params <- names(vectorPparams[, !is.na(vectorPparams=="shared") & vectorPparams=="shared"])
    names.of.specific.params <- names(vectorPparams[, !is.na(vectorPparams=="shared") & vectorPparams=="specific"])
    
    # shared
    vector.of.shared.params <- numeric(0)
    if(length(names.of.shared.params) > 0){
      vector.of.shared.params <- setNames(object = as.numeric(vectorPparams[, paste0(unit.names[1], names.of.shared.params)]),
               nm = names.of.shared.params)
    }
    # specific
    if(length(names.of.specific.params) > 0){
      matrix.of.specific.params <- NULL
      for (i.u in 1:length(unit.names)) {
        matrix.of.specific.params <- cbind(matrix.of.specific.params,
                                           as.numeric(vectorPparams[, paste0(unit.names[i.u], names.of.specific.params)]))
      }
      dimnames(matrix.of.specific.params) <-
        list(names.of.specific.params, unit.names)
    } else {
      matrix.of.specific.params <- 
        array(
        data = numeric(0), 
        dim = c(0, length(unit.names)), 
        dimnames = list(NULL, unit.names))
    }
    
    # combine into list
    listpParams <- list(shared = vector.of.shared.params,
                        specific = matrix.of.specific.params)
    # return
    listpParams
  }
