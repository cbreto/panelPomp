#' @include runif_EstimationScale.R
NULL

#' @title Interpret shortcuts for \code{sQuote()}s and \code{dQuote()}s in 
#' character objects
#' @description Concatenate character objects and replace singles quotes with 
#' \code{sQuote()}s and asterisks with \code{dQuote()}s: \code{sQuote("x")} and
#' \code{dQuote("x")} can be written as just ''x'' and *x*.
#' @param ... objects to be passed to \code{strsplit}.
#' @examples
#' wQuotes("in ''fn'': *object* is 'a' required argument")
#' paste0("in ",sQuote("fn"),": ",dQuote("object")," is 'a' required argument")
#' @export
wQuotes<- function (...) {
  char <- do.call(paste0,list(...)) ## combine arguments
  swap <- list(
    list(split="''",what="sQuote"),
    list(split="*",what="dQuote"))
  for (sw in seq_along(swap)) {
    chnks <- strsplit(char,split=swap[[sw]]$split,fixed=TRUE)[[1]] ## split char
    if (length(chnks)>1) { ## if any pattern
      ## check if initial 
      ODD <- FALSE
      if (chnks[1]=="") {
        ODD <- TRUE
        chnks <- chnks[-1]
      }
      ## replace by what
      ns <- seq(ifelse(ODD,1,2),length(chnks),by=2)
      #for (n in ns) chnks[n] <- paste0(sQuote(chnks[n]))
      for (n in ns) chnks[n] <- paste0(do.call(swap[[sw]]$what,list(chnks[n])))
    }
    char <- do.call(paste0,as.list(chnks))
  }
  char
}
