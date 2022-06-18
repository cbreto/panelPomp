
##' panelPomp plotting facilities
##'
##' Diagnostic plots for each unit in a panelPomp
##'
##' @name plot
##' @rdname plot
##' @aliases plot
##' @importFrom graphics lines par
##' @importFrom grDevices dev.interactive
##'
NULL

setGeneric("plot")

setClassUnion(
  "panelPomp_plottable",
  c(
    "panelPomp","pfilterd.ppomp","mif2d.ppomp"
  )
)

##' @rdname plot
##' @param x the object to plot
##' @param variables optional character; names of variables to be displayed
##' @param panel function of prototype \code{panel(x, col, bg, pch, type, ...)} which gives the action to be carried out in each panel of the display.
##' @param nc the number of columns to use.
##' Defaults to 1 for up to 4 series, otherwise to 2.
##' @param yax.flip logical;
##' if TRUE, the y-axis (ticks and numbering) should flip from side 2 (left) to 4 (right) from series to series.
##' @param mar,oma the \code{\link{par}} \code{mar} and \code{oma} settings.
##' Modify with care!
##' @param axes logical; indicates if x- and y- axes should be drawn
##' @param \dots ignored or passed to low-level plotting functions
##' @export
setMethod(
  "plot",
  signature=signature(x="panelPomp_plottable"),
  definition=function (x, variables,
    panel = lines, nc = NULL, yax.flip = FALSE,
    mar = c(0, 5.1, 0, if (yax.flip) 5.1 else 2.1),
    oma = c(6, 0, 5, 0), axes = TRUE, ...) {
  U <- length(x)
  if(U>1) {
    op <- par(ask=dev.interactive(orNone=TRUE))
    on.exit(par(op))
  }
  for(u in 1:U) plot(x=x[[u]],variables=variables,
      panel=panel,nc=nc,yax.flip=yax.flip,
      mar=mar,oma=oma,axes=axes,main=names(x)[u],...)
  }
)

