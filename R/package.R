## package description

#' @docType package
#' @name panelPomp-package
#' @title Inference for PanelPOMPs (Panel Partially Observed Markov Processes)
#'
#' @description The \pkg{panelPomp} package provides facilities for inference 
#' on panel data using panel partially-observed Markov process 
#' (\acronym{PanelPOMP}) models. To do so, it relies on and extends a number 
#' of facilities that the \pkg{pomp} package provides for inference on time 
#' series data using partially-observed Markov process (\acronym{POMP}) models. 
#' 
#' The \pkg{panelPomp} package extends to panel data some of the capabilities 
#' of the \pkg{pomp} package to fit nonlinear, non-Gaussian dynamic models. 
#' This is done accomodating both fixed and random effects. Currently, the 
#' focus is on likelihood-based approaches. In addition to these 
#' likelihood-based tools, \pkg{panelPomp} also provides a framework under 
#' which alternative statistical methods for \acronym{PanelPOMP} models can be 
#' developed (very much like \pkg{pomp} provides a platform upon which 
#' statistical inference methods for \acronym{POMP} models can be implemented).
#'
#' @section Data analysis using \pkg{panelPomp}:
#' The first step in using \pkg{panelPomp} is to encode one's model(s) and data
#'  in objects of class \code{panelPomp}.
#' One does this via a call to the \link[=panelPomp]{panelPomp} constructor 
#' function.
#' 
#' \pkg{panelPomp} version 
#' \Sexpr[echo=FALSE,stage=build,results=text]{packageDescription("panelPomp",fields="Version")} 
#' provides algorithms for
#' \enumerate{
#' \item particle filtering of panel data (AKA sequential Monte Carlo or 
#' sequential importance sampling), as proposed in Breto, Ionides and King 
#' (2018). This reference provides the fundamental theoretical support for the 
#' averaging of Monte Carlo replicates of panel unit likelihoods as implemented 
#' in \pkg{panelPomp}; see \code{\link{pfilter}}
#' \item the panel iterated filtering method of Breto, Ionides and King 
#' (2018). This reference provides the fundamental theoretical support for the 
#' extensions of the iterated filtering ideas of Ionides et al. (2006, 2011, 
#' 2015) to panel data as implemented in \pkg{panelPomp}; see 
#' \code{\link{mif2}}
#' }
#' The package also provides various tools for handling and extracting 
#' information on models and data.
#' 
#' @section Extending the \pkg{pomp} platform for developing inference tools:
#' \pkg{panelPomp} extends to panel data the general interface to the 
#' components of \acronym{POMP} models provided by \pkg{pomp}. In doing so, it 
#' contributes to the goal of the \pkg{pomp} project of facilitating the 
#' development of new algorithms in an environment where they can be tested 
#' and compared on a growing body of models and datasets.
#' 
#' @section Comments, bug reports, and requests:
#' Contributions are welcome, as are suggestions for improvement, feature 
#' requests, and bug reports.
#' Please submit these via the \href{https://github.com/cbreto/panelPomp/issues}{panelPomp issues page}.
#' We particularly welcome minimal working examples displaying uninformative, 
#' misleading or inacurate error messages. We also welcome suggestions for 
#' clarifying obscure passages in the documentation. Help requests are welcome, 
#' but please consider before sending requests whether they are regarding the 
#' use of \pkg{panelPomp} or that of \pkg{pomp}. For help with \pkg{pomp}, 
#' please visit 
#' \href{https://kingaa.github.io/pomp/FAQ.html#how-can-i-submit-an-effective-request-for-help}{\pkg{pomp}'s FAQ}.
#' 
#' @section Documentation:
#' Examples are provided via the \code{\link{panelPompExample}} function.
#' To see a list of the examples included in \pkg{panelPomp}, use
#' \preformatted{panelPompExample()}
#' To see a list of the examples included both in \pkg{panelPomp} and 
#' \pkg{pomp}, use \pkg{pomp}'s
#' \preformatted{pompExample()}
#' 
#' @section License:
#' \pkg{panelPomp} is provided under the \acronym{MIT} License.
#' 
#' @references \breto2018
#' @author Carles Breto
#' 
#' @seealso \link[=pomp2-package]{pomp package}, \link{panelPomp}
#' 
#' @keywords models datasets ts
#' 
#' @import methods
#' @import pomp2
#' @useDynLib panelPomp, .registration=TRUE
#' @importFrom stats dnorm runif setNames var
#' @importFrom utils tail
NULL        # replacing NULL by "_PACKAGE" results in roxygen2 adding an 
            # \alias{} with the package name, conflicting with functions named 
            # after the package
