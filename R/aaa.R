#' @include package.R
NULL

.onAttach <- function (...) {
  pompExDir <- getOption("pomp.examples")
  pPompExDir <- getOption("panelPomp.examples")
  newDir <- system.file("examples",package="panelPomp")
  if (!newDir%in%pompExDir) 
    options(pomp.examples=c(pomp=pompExDir,pomp=newDir))
  if (!newDir%in%pPompExDir) 
    options(panelPomp.examples=c(panelPomp=pPompExDir,panelPomp=newDir))
}

.onDetach <- function (...) {
  pompExDir <- getOption("pomp.examples")
  pPompExDir <- getOption("panelPomp.examples")
  newDir <- system.file("examples",package="panelPomp")
  pompExDir <- pompExDir[pompExDir!=newDir]
  if (identical(unname(pompExDir),character())) pompExDir <- NULL
  pPompExDir <- pPompExDir[pPompExDir!=newDir]
  if (identical(unname(pPompExDir),character())) pPompExDir <- NULL
  options(pomp.examples=pompExDir)
  options(panelPomp.examples=pPompExDir)
}

## Uniform random draws in the transformed scale: give centers and widths
runif.EstimationScale <-
  function(centers, widths,
           toEstimationScale.fn = log, fromEstimationScale.fn = exp) {
    transformed.centers <- toEstimationScale.fn(centers)
    fromEstimationScale.fn(runif(
      n = length(centers), min = transformed.centers - widths*0.5,
      max = transformed.centers + widths*0.5
    ))
  }

test <- function (expr1, expr2, all, env, verbose = TRUE) {
  # expr1: expression to be try(,sil=T)[1]d; can be a quote() to avoid 
  #     evaluation by the function
  # expr2: optional; compare to 'expr1' (via identical) and c() logical result
  #     to object specified via arguments 'all' and 'env' (see below)
  # all: name of the vector to accumulate logical test results
  # env: name of the environment where 'all' should be modified
  # verbose: optional; should the result be returned?
  ep <- paste0("in ",sQuote("test"),": ")
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

#' @title Interpret shortcuts for \code{sQuote()}s and \code{dQuote()}s in 
#' character objects
#' @description Concatenate character objects and replace singles quotes with 
#' \code{sQuote()}s and asterisks with \code{dQuote()}s: \code{sQuote("x")} and
#' \code{dQuote("x")} can be written as just ''x'' and *x*.
#' @param ... objects to be passed to \code{strsplit}.
#' @keywords internal
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
      for (n in ns) chnks[n] <- paste0(do.call(swap[[sw]]$what,list(chnks[n])))
    }
    char <- do.call(paste0,as.list(chnks))
  }
  char
}

# The `pompExample` function has been eliminated in pomp version 2. For the sake 
# of a more timely release of a pomp compatible version of panelPomp, the last
# version of `pompExample` (as written by Aaron A. King) is temporarily 
# reproduced here. panelPomp will also soon move away from this approach of 
# loading examples.
pompExample <- function (example, ..., show = FALSE, envir = .GlobalEnv) {
  example <- as.character(substitute(example))
  ep <- paste0("in ",sQuote("panelPomp:::pompExample"),": ")
  pomp.dir <- system.file("examples",package="pomp")
  exampleDirs <- getOption("pomp.examples",default=pomp.dir)
  names(exampleDirs) <- exampleDirs
  show <- as.logical(show)
  if (example=="") {
    avlbl <- lapply(exampleDirs,list.files,pattern=".+?R$")
    avlbl <- lapply(avlbl,function(x) gsub("\\.R$","",x))
    for (dir in exampleDirs) {
      cat("examples in ",dir,":\n",sep="")
      print(avlbl[[dir]])
    }
  } else {
    evalEnv <- list2env(list(...))
    file <- c(lapply(exampleDirs,list.files,
                     pattern=paste0(example,".R"),
                     full.names=TRUE),
              recursive=TRUE)
    if (length(file)<1) {
      stop(ep,"cannot find file ",
           sQuote(paste0(example,".R")),call.=FALSE)
    }
    #if (length(file)>1) {
    #  warning(ep,"using ",sQuote(file[1])," from ",sQuote(names(file)[1]),call.=FALSE)
    #}
    if (show) {
      file.show(file[1])
      return(invisible(NULL))
    }
    objs <- source(file[1],local=evalEnv)
    if (is.null(envir)) {
      obj <- setNames(lapply(objs$value,get,envir=evalEnv),objs$value)
    } else if (is.environment(envir)) {
      for (i in seq_along(objs$value)) {
        assign(objs$value[i],
               get(objs$value[i],envir=evalEnv),
               envir=envir)
      }
      cat("newly created object(s):\n",objs$value,"\n")
      obj <- NULL
    } else {
      stop(ep,sQuote("envir")," must be an environment or NULL",call.=FALSE)
    }
    invisible(obj)
  }
}
