##' Simulations of a panel of partially observed Markov process
##'
##' \code{simulate} generates simulations of the state and measurement
##' processes.
##'
##' @name simulate
##' @rdname simulate
##' @author Edward L. Ionides
##' @param object  a \sQuote{panelPomp} object.
##' @param nsim The number of simulations to perform. Unlike the pomp
##' simulate method, all simulations share the same parameters.
##' @param shared Named vector of the shared paramters.
##' @param specific Matrix of unit-specific parameters, with a column
##' for each unit.
##' @return
##' A single panelPomp object (if nsim=1) or a list of panelPomp objects
##' (if nsim>1).
##'
NULL

##' @rdname simulate
#' @examples
#' simulate(panelRandomWalk())
#' @export
setMethod(
  "simulate",
  signature="panelPomp",
  definition=function (object,nsim = 1,shared,specific) {

  if (length(nsim)!=1 || !is.numeric(nsim) || !is.finite(nsim) || nsim < 1)
    pomp:::pStop_(sQuote("nsim")," must be a positive integer.")
  nsim <- as.integer(nsim)

  if (!missing(shared))  object@shared <- shared
  if (!missing(specific)) object@specific <- specific

  if (length(object@shared)==0 & length(object@specific)==0)
    pomp:::pStop_("at least one of shared and specific must be specified.")

  ppList <- lapply(1:nsim,function(n,pp1){
    pompList <- lapply(1:length(pp1),function(u,pp1)
      simulate(pp1[[u]]),pp1=pp1)
    names(pompList) <- names(pp1)
    panelPomp(pompList,shared=pp1@shared,specific=pp1@specific)
  },pp1=object)
  if(nsim==1)ppList <- ppList[[1]]
  ppList
}
)

