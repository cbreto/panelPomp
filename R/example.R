##  panelPompExample function

#' @include aaa.R
NULL

#' @title Construct \pkg{panelPomp} examples
#' @description \code{panelPompExample} constructs \code{panelPomp} objects 
#' that come with the \pkg{panelPomp} package.
#' @param example a \code{character} object specifying one of the examples that 
#' come with the \pkg{panelPomp} package. These examples can be listed using 
#' \code{panelPompExamples()}.
#' @details \code{panelPompExample} is related but different than its 
#' \pkg{pomp} counterpart \code{\link[pomp]{pompExample}}.
#' The \file{examples} directory in the installed package has some 
#' example files that can be listed using \code{panelPompExamples()}.
#' @return By default, a \code{panelPomp} object. If \code{envir} is an 
#' environment, this \code{panelPomp} object is created in that environment 
#' and named \code{example}.
#' @seealso \code{\link[pomp]{pompExample}}
#' @author Carles Breto and Aaron A. King
#' @export
panelPompExample <- function (example) {
  example <- as.character(substitute(example))
  objExample(example,pckg="panelPomp")
}

pompExample <- function (example) {
  example <- as.character(substitute(example))
  objExample(example,pckg="pomp")
}

objExample <- function (example, pckg) {
  ep <- wQuotes("in ''",pckg,"Example'': ")
  dirs <- getOption(paste0(pckg,".examples"))
  if (example=="") 
    for (dd in seq_along(dirs)) {
      cat(names(dirs)[dd]," examples in ",dirs[dd],":\n",sep="")
      print(lapply(lapply(dirs,list.files,pattern=".+?R$"),
                   function(x) gsub("\\.R$","",x))[[dd]])
    }
  else {
    localEnv <- new.env()
    exPath <- character()
    for (dd in seq_along(dirs)) {
      if (file.exists(paste0(dirs[dd],"/",example,".R"))) 
        exPath <- c(exPath,dirs[dd])
    }
    if (length(exPath)<1) 
      stop(wQuotes(ep,"cannot find file ''",example,".R''"),call.=FALSE)
    if (length(exPath)>1) 
      warning(wQuotes(ep,"multiple instances of file ''",example,".R'' were ",
                      "found; the one from ''",exPath[1],"'' is being used"),
              call.=FALSE)
    local.obj <- source(paste0(exPath[1],"/",example,".R"),local=localEnv)
    assign("obj",get(local.obj$value,envir=localEnv))
    obj
  }
}
