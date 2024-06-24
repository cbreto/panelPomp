% unlike in LaTeX, new lines within \newcommand end the command (even using %)

\newcommand{\Atchade}{Atchad\ifelse{latex}{\out{\'e}}{\ifelse{html}{\out{&eacute;}}{e}}}

\newcommand{\Breto}{Bret\ifelse{latex}{\out{\'o}}{\ifelse{html}{\out{&oacute;}}{o}}}

\newcommand{\inheritDes}{The \pkg{panelPomp} function \code{#1} inherits arguments from the \pkg{pomp} function with the same name. The argument description below is taken directly from the \pkg{pomp} documentation}

\newcommand{\inheritArg}{(optional) named list of same length as \code{object} with unit-specific values for the \code{#1} argument of the \pkg{pomp} function \code{#2}. If not specified, the \code{#1} argument must be specified and will be used for all units}

\newcommand{\pparamsReturn}{When given objects of class \code{panelPomp}, \code{pparams()} returns a named list with two elements: \code{shared}, which is a named \code{numeric} vector, and \code{specific}, which is a \code{matrix} with parameter names in its row names and panel unit names in its column names.}

\newcommand{\unitLogLikReturn}{When given objects of class \code{pfilterd.ppomp}, \code{unitLoglik()} returns a \code{numeric} vector.}

\newcommand{\unitobjectsReturn}{When given objects of class \code{panelPomp}, \code{unit_objects()} returns a \code{list} of \code{pomp} objects.}
