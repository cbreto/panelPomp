#' #' Create design matrix for panelPomp calculations
#'
#' These functions are useful for generating design matrices for the exploration
#' of parameter space.
#'
#' @name panel-designs
#' @concept panel-designs
#'
#' @author Jesse Wheeler, Aaron A. King
NULL

#' \code{runif_panel_design} generates a design based on random samples from a
#' multivariate uniform distribution.
#'
#' @param lower,upper named numeric vectors giving the lower and upper bounds
#'    of the ranges, respectively.
#' @param nseq Total number of points requested
#' @param specific_names Character vector containing the names of unit-specific
#'    parameters. This argument must be used in conjunction with the argument
#'    \code{unit_names}; it is used if the search bounds for all unspecified
#'    unit-specific parameters are the same.
#' @param unit_names Character vector containing the names of the units of the
#'    panel. If not used in conjunction with \code{unit_names} this argument
#'    is ignored.
#'
#' @return \code{runif_panel_design} returns a \code{data.frame} object with
#'    \code{nseq} rows. Each row corresponds to a parameter set drawn randomly
#'    from a multivariate uniform distribution specified by the \code{lower},
#'    \code{upper}, \code{specific_names} and \code{unit_names} arguments.
#' @export
#'
#' @examples
#' runif_panel_design(
#'   lower = c('a' = 0, 'b' = 10, 'a[u2]' = 0.5),
#'   upper = c('a' = 1, 'b' = 15, 'a[u2]' = 0.75),
#'   specific_names = c('a'),
#'   unit_names = paste0(rep('u', 5), 1:5),
#'   nseq = 10
#' )
#' @rdname panel-designs
runif_panel_design <- function (
    lower = numeric(0), upper = numeric(0), nseq, specific_names, unit_names
) {

  ep <- wQuotes("in ''runif_panel_design'': ")

  # Checks made in pomp::runif_design
  if (length(lower)!=length(upper))
    stop(wQuotes(ep,"''lower'' and ''upper'' must have the same length","."),call.=FALSE)
  lnames <- names(lower)
  if (is.null(lnames))
    stop(wQuotes(ep,"''lower'' and ''upper'' must be named vectors","."),call.=FALSE)
  if (!all(sort(lnames)==sort(names(upper))))
    stop(wQuotes(ep,"names of ''lower'' and ''upper'' must match","."),call.=FALSE)
  upper <- upper[lnames]
  if (!all(upper>=lower))
    stop(wQuotes(ep,"upper values should be at least as large as lower ones","."),call.=FALSE)
  nseq <- as.integer(nseq)
  if (nseq < 0)
    stop(wQuotes(ep,"''nseq'' must be greater than zero","."),call.=FALSE)

  if (missing(specific_names) && missing(unit_names)) {
    # Do as pomp::runif_design
    y <- matrix(
      data=runif(n=nseq*length(lower),min=lower,max=upper),
      nrow=nseq,ncol=length(lower),
      byrow=TRUE
    )
    colnames(y) <- lnames
    return(as.data.frame(y))
  } else {
    if (missing(specific_names) | missing(unit_names))
      stop(wQuotes(ep,"If used, both ''specific_names'' and ''unit_names'' must be provided","."),call.=FALSE)

    if (any(!specific_names %in% lnames))
      stop(wQuotes(ep,"No bounds were given for some parameters in ''specific_names''","."),call.=FALSE)

    lwr_tmp <- lower[!lnames %in% specific_names]
    upr_tmp <- upper[!lnames %in% specific_names]
    upr_tmp <- upr_tmp[names(lwr_tmp)]

    lwr_spec <- rep(
      lower[lnames %in% specific_names], each = length(unit_names)
    )

    unit_labs <- paste0("[", rep(unit_names, length(unique(names(lwr_spec)))), "]")
    names(lwr_spec) <- paste0(names(lwr_spec), unit_labs)

    upr_spec <- rep(
      upper[lnames %in% specific_names], each = length(unit_names)
    )

    names(upr_spec) <- names(lwr_spec)

    lower <- c(lwr_tmp, lwr_spec[!names(lwr_spec) %in% names(lwr_tmp)])
    upper <- c(upr_tmp, upr_spec[!names(lwr_spec) %in% names(lwr_tmp)])

    lower <- lower[sort(names(lower))]
    upper <- upper[sort(names(upper))]

    y <- matrix(
      data=runif(n=nseq*length(lower),min=lower,max=upper),
      nrow=nseq,ncol=length(lower),
      byrow=TRUE
    )
    colnames(y) <- names(lower)
    return(as.data.frame(y))
  }
}
