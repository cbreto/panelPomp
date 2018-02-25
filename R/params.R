## functions to manipulate params

#' @include panel_logmeanexp.R
NULL

#' @title Convert to and from a \code{panelPomp} object \code{pParams} slot format and a one-row \code{data.frame} 
#' @description These facilitate keeping a record of evaluated log likelihoods.
#' @param vec_pars A one-row \code{data.frame} with format matching that of the 
#' output of \link{toVectorPparams}.
#' @param pParams A list with the format of the \code{pParams} slot of \code{panelPomp} objects.
#' @name params
NULL

#' @rdname params
#' @export
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

## Go to list-form pparams from matrix specification
toListPparams <- function(
  matrixPparams,
  names.in.vector,
  vector.position.in.listPparams,
  vector.name.in.listPparams,
  matrix.name.in.listPparams
) {
  output <- as.vector(c(NA,NA), mode = "list")
  # fill vector in
  output[[vector.position.in.listPparams]] <- matrixPparams[names.in.vector,1]
  # fill matrix in
  name.logicals.for.matrix <- !rownames(matrixPparams) %in% names.in.vector
  output[[ifelse(vector.position.in.listPparams==1,2,1)]] <-
    if(any(name.logicals.for.matrix)) {
      matrixPparams[name.logicals.for.matrix, , drop = FALSE]} else {
        # fill empty matrix in
        array(
          data = numeric(0), 
          dim = c(0, dim(matrixPparams)[2]),
          dimnames = list(NULL, dimnames(matrixPparams)[[2]])
        )
      }
  names(output)[vector.position.in.listPparams] <- vector.name.in.listPparams
  names(output)[ifelse(vector.position.in.listPparams==1,2,1)] <- matrix.name.in.listPparams
  output
}

## Go to matrix-form pparams from list specification
toMatrixPparams <- function(listPparams) {
  common.params <- listPparams[[which(sapply(listPparams, is.vector))]]
  specific.params <- listPparams[[which(sapply(listPparams, is.matrix))]]
  
  U <- dim(specific.params)[2]
  matrixPparams <- rbind(
    matrix(
      rep(x = common.params, times = U),
      ncol = U,
      dimnames = list(names(common.params), NULL)
    ),
  specific.params)
  matrixPparams
}

#' @rdname params
#' @export
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
