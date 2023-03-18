% unlike in LaTeX, new lines within \newcommand end the command (even using %)

\newcommand{\Atchade}{Atchad\ifelse{latex}{\out{\'e}}{\ifelse{html}{\out{&eacute;}}{e}}}

\newcommand{\Breto}{Bret\ifelse{latex}{\out{\'o}}{\ifelse{html}{\out{&oacute;}}{o}}}

\newcommand{\inheritDes}{The \pkg{panelPomp} function \code{#1} inherits arguments from the \pkg{pomp} function with the same name. The argument description below is taken directly from the \pkg{pomp} documentation}

\newcommand{\inheritArg}{(optional) named list of same length as \code{object} with unit-specific values for the \code{#1} argument of the \pkg{pomp} function \code{#2}. If not specified, the \code{#1} argument must be specified and will be used for all units}
