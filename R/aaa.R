#' @include package.R
NULL

#' @title Interpret shortcuts for \code{sQuote()}s and \code{dQuote()}s in
#' character objects
#' @description Concatenate character objects and replace double quotes with
#' \code{sQuote()} (write \code{''x''} instead of \code{dQuote("x")}) and
#' replace asterisks with \code{dQuote()}.
#' @param ... objects to be passed to \code{strsplit}.
#' @return
#' A \code{character} object.
#' @keywords internal
#' @examples
#' paste0("in ",sQuote("fn_name"),": ",dQuote("object")," is 'a' required argument")
#' wQuotes("in ''fn_name'': *object* is 'a' required argument") # same but shorter
#' @export
#' @author Carles \Breto
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
      for (n in ns) chnks[n] <- paste0(do.call(swap[[sw]]$what,list(chnks[n])))
    }
    char <- do.call(paste0,as.list(chnks))
  }
  char
}

## Uniform random draws in the transformed scale: give centers and widths
## @author Carles \Breto
runif.EstimationScale <-
  function(centers, widths,
           toEstimationScale.fn = log, fromEstimationScale.fn = exp) {
    transformed.centers <- toEstimationScale.fn(centers)
    fromEstimationScale.fn(runif(
      n = length(centers), min = transformed.centers - widths*0.5,
      max = transformed.centers + widths*0.5
    ))
  }

## @author Carles \Breto
test <- function (expr1, expr2, all, env, verbose = TRUE) {
  # expr1: expression to be try(,sil=T)[1]d; can be a quote() to avoid
  #     evaluation by the function
  # expr2: optional; compare to 'expr1' (via identical) and c() logical result
  #     to object specified via arguments 'all' and 'env' (see below)
  # all: name of the vector to accumulate logical test results
  # env: name of the environment where 'all' should be modified
  # verbose: optional; should the result be returned?
  ep <- panelPomp::wQuotes("in ''test'': ")
  if (!exists(all,envir=env))
    stop(paste0(ep,"missing vector to accumulate logical test results."),
         call.=FALSE)
  tryexpr1 <- try(eval(expr1),silent=TRUE)
  if (is(tryexpr1,"try-error")) tryexpr1 <- tryexpr1[1]
  PASSES <- tryexpr1
  if (!missing(expr2)) {
    tryexpr2 <- try(eval(expr2),silent=TRUE)
    if (is(tryexpr2,"try-error")) tryexpr2 <- tryexpr2[1]
    PASSES <- identical(tryexpr1,tryexpr2)
    assign(all,value=c(get(all,envir=env),PASSES),envir=env)
  }
  if (verbose) PASSES
}
