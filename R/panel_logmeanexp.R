#' @include panel_loglik.R
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
  logmeanexps <-apply(x,MARGIN=MARGIN,FUN=logmeanexp,se=TRUE)
  summed <- sum(logmeanexps[1,])
  if (!se) {
    summed
  } else {
    c(summed,se=sqrt(sum(unname(logmeanexps[2,])^2)))
  }
}
