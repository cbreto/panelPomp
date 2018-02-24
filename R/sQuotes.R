#' @include runif_EstimationScale.R
NULL

#' @title Write \code{sQuote()}s as singles quotes (') in character objects
#' @description Concatenate character objects and replace singles quotes (') with 
#' \code{sQuote()}s: \code{sQuote('x')} can be written as just 'x'.
#' @param ... character objects (or other objects to be passed to 
#' \code{strsplit(paste0(...),"'")[[1]]}).
#' @examples
#' sQuotes("in 'fn': 'object' is a required argument")
#' paste0("in ",sQuote('fn'),": ",sQuote('object')," is a required argument")
#' @export
sQuotes <- function (...) {
  char <- do.call(paste0,list(...)) ## combine arguments
  chnks <- strsplit(char,"'")[[1]] ## split char
  if (length(chnks)>1) { ## if any quote
    ## check if initial 
    ODD <- FALSE
    if (chnks[1]=="") {
      ODD <- TRUE
      chnks <- chnks[-1]
    }
    ## replace by sQuote()s
    ns <- seq(ifelse(ODD,1,2),length(chnks),by=2)
    for (n in ns) chnks[n] <- paste0(sQuote(chnks[n]))
  }
  do.call(paste0,as.list(chnks))
}
