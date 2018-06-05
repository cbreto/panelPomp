## ABC algorithm codes

#' @include mif2_methods.R
NULL

#' @title PABC: Panel Approximate Bayesian Computation
#' @description Tools for applying Approximate Bayesian Computation algorithms
#' to panel data.
#' @inheritParams coef,panelPomp-method
#' @inheritParams mif2,panelPomp-method
#' @inheritParams pomp::abc
#' @param pproposal optional list; list of same length as \code{object} with
#' unit-specific values for the \code{proposal} argument of the \pkg{pomp}
#' function \code{abc}. If not specified, the \code{proposal} argument must be
#' specified and will be used for all units.
#' @param pprobes optional list; list of same length as \code{object} with
#' unit-specific values for the \code{probe} argument of the \pkg{pomp}
#' function \code{abc}. If not specified, the \code{probe} argument must be
#' specified and will be used for all units.
#' @param pscale optional list; list of same length as \code{object} with
#' unit-specific values for the \code{scale} argument of the \pkg{pomp}
#' function \code{abc}. If not specified, the \code{scale} argument must be
#' specified and will be used for all units.
#' @name abc
#' @references \tavare1997
#' 
#' \sisson2007
#' 
#' \sisson2009
#' @seealso \pkg{pomp}'s abc at \link[=abc,pomp-method]{abc}
NULL

#' @rdname abc
#' @export

## define the abc class
setClass(
  'abcd.ppomp',
  contains='panelPomp',
  slots=c(
    pars = 'character',
    Nabc = 'integer',
    accepts = 'integer',
    probes='list',
    scale = 'numeric',
    epsilon = 'numeric',
    proposal = 'function',
    conv.rec = 'matrix'
  ),
  prototype=prototype(
    pars=character(),
    Nabc=0L,
    accepts=0L,
    probes=list(),
    scale=numeric(),
    epsilon=1.0,
    proposal=function (...)
      stop(wQuotes("in ''abc'': proposal not specified"),call.=FALSE),
    conv.rec=array(dim=c(0,0))
  )
)

abc.internal <- function (object, Nabc, start, proposal, probes, epsilon, scale,
                          verbose = FALSE,.ndone = 0L,.accepts = 0L,
                          .getnativesymbolinfo = TRUE,
                          pproposal=pproposal,pprobes=pprobes,pscale=pscale,
                          ...) {
  
  ep <- wQuotes("in ''abc'': ")
  et <- wQuotes(" (panelPomp:::abc.internal)")
  
  # make lists for entire panel out of single arguments: pproposal, pprobes and pscale
  object_passed <- object
  object <- as(object[[1]],'pomp')
  start <- coef(object)
  
  
  gnsi <- as.logical(.getnativesymbolinfo)
  Nabc <- as.integer(Nabc)
  .ndone <- as.integer(.ndone)
  .accepts <- as.integer(.accepts)
  epsilon <- as.numeric(epsilon)
  epssq <- epsilon*epsilon
  verbose <- as.logical(verbose)
  
  #pompLoad(object,verbose=verbose)
  
  if (length(start)==0)
    stop(ep,sQuote("start")," must be specified if ",
         sQuote("coef(object)")," is NULL",et,
         call.=FALSE)
  
  start.names <- names(start)
  if (is.null(start.names))
    stop(ep,sQuote("start")," must be a named vector",et,call.=FALSE)
  
  if (!is.function(proposal))
    stop(ep,sQuote("proposal")," must be a function",et,call.=FALSE)
  
  ## test proposal distribution
  theta <- tryCatch(
    proposal(start,.n=0),
    error = function (e) {
      stop(ep,"in proposal function: ",conditionMessage(e),et,call.=FALSE)
    }
  )
  if (is.null(names(theta)) || !is.numeric(theta) || any(names(theta)==""))
    stop(ep,sQuote("proposal")," must return a named numeric vector",et,call.=FALSE)
  
  if (!is.list(probes)) probes <- list(probes)
  if (!all(sapply(probes,is.function)))
    stop(ep,sQuote("probes")," must be a function or a list of functions",et,call.=FALSE)
  if (!all(sapply(probes,function(f)length(formals(f))==1)))
    stop(ep,"each probe must be a function of a single argument",et,call.=FALSE)
  
  if (verbose) {
    cat("performing",Nabc,"ABC iteration(s)\n")
  }
  
  theta <- start
  log.prior <- tryCatch(
    dprior(object,params=theta,log=TRUE,.getnativesymbolinfo=gnsi),
    error = function (e) {
      stop(ep,sQuote("dprior")," error: ",conditionMessage(e),et,call.=FALSE)
    }
  )
  if (!is.finite(log.prior))
    stop(ep,"inadmissible value of ",sQuote("dprior")," at parameters ",
         sQuote("start"),et,call.=FALSE)
  ## we suppose that theta is a "match",
  ## which does the right thing for continue() and
  ## should have negligible effect unless doing many short calls to continue()
  
  conv.rec <- matrix(
    data=NA,
    nrow=Nabc+1,
    ncol=length(theta),
    dimnames=list(
      iteration=seq(from=0,to=Nabc,by=1),
      variable=names(theta)
    )
  )
  
  ## apply probes to data
  datval <- tryCatch(
    .Call("apply_probe_data",object,probes#,
          #PACKAGE="pomp"# This argument seems to be intended to be used by
          # 'pomp' to be able to make sure that it uses its own 'apply_probe_sim'
          ),
    error = function (e) {
      stop(ep,"in ",sQuote("apply_probe_data"),": ",conditionMessage(e),et,
           call.=FALSE)
    }
  )
  
  conv.rec[1,names(theta)] <- theta
  
  for (n in seq_len(Nabc)) { # main loop
    
    theta.prop <- tryCatch(
      proposal(theta,.n=n+.ndone,.accepts=.accepts,verbose=verbose),
      error = function (e) {
        stop(ep,"in proposal function: ",conditionMessage(e),et,call.=FALSE)
      }
    )
    log.prior.prop <- tryCatch(
      dprior(object,params=theta.prop,log=TRUE),
      error = function (e) {
        stop(ep,sQuote("dprior")," error: ",conditionMessage(e),et,call.=FALSE)
      }
    )
    
    if (is.finite(log.prior.prop) &&
        runif(1) < exp(log.prior.prop-log.prior)) {
      
      ## compute the probes for the proposed new parameter values

            simval <- tryCatch(
        .Call(
          "apply_probe_sim",
          object=object,
          nsim=1L,
          params=theta.prop,
          seed=NULL,
          probes=probes,
          datval=datval#,
          #PACKAGE="pomp" # This argument seems to be intended to be used by
          # 'pomp' to be able to make sure that it uses its own 'apply_probe_sim'
        ),
        error = function (e) {
          stop(ep,"in ",sQuote("apply_probe_sim"),": ",conditionMessage(e),et,
               call.=FALSE)
        }
      )
      
      ## ABC update rule
      distance <- sum(((datval-simval)/scale)^2)
      if( (is.finite(distance)) && (distance<epssq) ){
        theta <- theta.prop
        log.prior <- log.prior.prop
        .accepts <- .accepts+1L
      }
      
    }
    
    ## store a record of this iteration
    conv.rec[n+1,names(theta)] <- theta
    if (verbose && (n%%5==0))
      cat("ABC iteration",n+.ndone,"of",Nabc+.ndone,
          "completed\nacceptance ratio:",
          round(.accepts/(n+.ndone),3),"\n")
  }
  
  pars <- apply(conv.rec,2,function(x)diff(range(x))>0)
  pars <- names(pars[pars])
  
  #pompUnload(object,verbose=verbose)

  new(
    'abcd.ppomp',
    object_passed,
    pars=pars,
    Nabc=Nabc,
    accepts=.accepts,
    probes=probes,
    scale=scale,
    epsilon=epsilon,
    proposal=proposal,
    conv.rec=conv.rec
  )
  
}

#' @rdname abc
#' @export
setMethod(
  "abc",
  signature=signature(object="panelPomp"),
  definition=function (object, Nabc = 1, start, proposal, probes, scale,
                       epsilon,verbose = getOption("verbose"),
                       pproposal=NULL, pprobes=NULL, pscale=NULL,
                       ...) {
    
    ep <- wQuotes("in ''abc'': ")
    et <- wQuotes(" (''abc,panelPomp-method'')")
    
    if (missing(start))
      start <- coef(object)
    
    if (missing(proposal)) proposal <- NULL
    
    if (is.null(proposal))
      stop(ep,sQuote("proposal")," must be specified",et,call.=FALSE)
    
    if (missing(probes))
      stop(ep,sQuote("probes")," must be specified",et,call.=FALSE)
    
    if (missing(scale))
      stop(ep,sQuote("scale")," must be specified",et,call.=FALSE)
    
    if (missing(epsilon))
      stop(ep,"abc match criterion, ",sQuote("epsilon"),
           ", must be specified",et,call.=FALSE)
    
    abc.internal(
      object=object,
      Nabc=Nabc,
      start=start,
      proposal=proposal,
      probes=probes,
      scale=scale,
      epsilon=epsilon,
      verbose=verbose,
      pproposal=pproposal,
      pprobes=pprobes,
      pscale=pscale
    )
  }
)
