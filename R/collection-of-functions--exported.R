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