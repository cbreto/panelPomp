#' @include misc-unexported.R
NULL

## miscellanea of exported functions

### fromVectorPparams
#' fromVectorPparams.
#'
#' fromVectorPparams.
#'
#' Convert back to the format of the \code{pParams} slot \code{panelPomp} objects from a one-row \code{data.frame} (e.g., the row of a record of evaluated likelihoods with the maximum likelihood).
#'
#' @param vec_pars A one-row \code{data.frame} with format matching that of the output of \link{toVectorPparams}.
#' 
#' @export
#'
fromVectorPparams <- function(vec_pars) {
  # Extract unit, shared, and specific names
  sh_nms <- names(vec_pars[,!is.na(vec_pars=="shared")&vec_pars=="shared"])
  sp_nms <- names(vec_pars[,!is.na(vec_pars=="shared")&vec_pars=="specific"])
  u_nms <- names(vec_pars)[vec_pars=="unit_name"]
  # shared
  sh_pars <- if(length(sh_nms)>0) {
    sh_pars <- setNames(as.numeric(vec_pars[,paste0(u_nms[1],sh_nms)]),nm=sh_nms)
  } else {
    numeric(0)
  }
  # specific
  if(length(sp_nms)>0) {
    mat_sps <- NULL
    for (i.u in seq_len(length(u_nms))) {
      mat_sps <- cbind(
        mat_sps,
        as.numeric(vec_pars[, paste0(u_nms[i.u],sp_nms)])
      )
    }
    dimnames(mat_sps) <- list(sp_nms,u_nms)
  } else {
    mat_sps <- array(numeric(),dim=c(0,length(u_nms)),dimnames=list(NULL,u_nms))
  }
  # return
  list(shared=sh_pars,specific=mat_sps)
}

#' Get column.
#'
#' Subset matrix dropping dimension but without dropping rownames (which is R's default).
#'
#' @param matrix matrix.
#' @param rows numeric; rows to subset; like with `[`, this argument can be left empty to designate all rows.
#' @param col numeric; single column to subset.
#' 
#' @export
#'
get_col <- function (matrix, rows, col) {
  stopifnot(is.matrix(matrix)&!missing(col))
  # if all rows, i.e., get.col(mtrx,,cl)
  if(all('rows'!=names(as.list(match.call())))) {
    matrix[,col]
  } else {
    setNames(matrix[rows,col],nm=rownames(matrix)[rows]
    )
  }
}

#' Get row.
#'
#' Subset matrix dropping dimension but without dropping colnames (which is R's default).
#'
#' @param matrix matrix.
#' @param row numeric; single row to subset.
#' @param cols numeric; columns to subset; like with `[`, this argument can be left empty to designate all columns.
#' @export
#'
get_row <- function (matrix, row, cols) {
  stopifnot(is.matrix(matrix)&!missing(row))
  # if all columns, i.e., get.col(mtrx,rw,)
  if(all('cols'!=names(as.list(match.call())))) {
    matrix[row,]
  } else {
    setNames(matrix[row,cols],nm=colnames(matrix)[cols]
    )
  }
}


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
  logmeanexps <-apply(x,MARGIN=MARGIN,FUN=logmeanexp,se=TRUE)
  summed <- sum(logmeanexps[1,])
  if (!se) {
    summed
  } else {
    c(summed,se=sqrt(sum(unname(logmeanexps[2,])^2)))
    }
}

### toVectorPparams
#' toVectorPparams.
#'
#' toVectorPparams.
#'
#' Convert from the format of the \code{pParams} slot of \code{panelPomp} objects to a one-row \code{data.frame}. This facilitates keeping a record of evaluated likeilhoods.
#'
#' @param pParams A list with the format of the \code{pParams} slot of \code{panelPomp} objects.
#' 
#' @export
#'
toVectorPparams <- function(pParams) {
  # rbind replicated shared parameters with matrix of specific parameters
  mat_pars <- rbind(
    matrix(
      rep(pParams$shared,
          times=ncol(pParams$specific)),
      ncol = ncol(pParams$specific),
      dimnames = list(names(pParams$shared), NULL)
    ),
    pParams$specific
  )
  # vectorize the matrix
  vec_pars <- setNames(
    as.vector(mat_pars),
    nm=paste0(rep(colnames(mat_pars), each = nrow(mat_pars)),
              rownames(mat_pars)))
  # Append info about ...
  #... nature of parameters (shared and specific), and ...
  par_typ <- setNames(
    c(rep("shared",times=length(pParams$shared)),
      rep("specific",times=nrow(pParams$specific))),
    nm=c(names(pParams$shared),rownames(pParams$specific)))
  # ... unit names
  u_nms <- setNames(
    rep("unit_name",ncol(mat_pars)),
    nm=colnames(pParams$specific)
  )
  # return
  merge(data.frame(t(par_typ),stringsAsFactors=FALSE),
        y=merge(data.frame(t(u_nms),stringsAsFactors=FALSE),
                y=data.frame(t(vec_pars))))
}
