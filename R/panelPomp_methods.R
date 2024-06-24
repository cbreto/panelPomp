## methods for panelPomp objects (other than workhorses: pfilter, mif2, etc.)

#' @include panelPomp.R
NULL

#' @name panelPomp_methods
#' @docType methods
#' @title Manipulating \code{panelPomp} objects
#' @description Tools for manipulating \code{panelPomp} objects.
#' @param object,x An object of class \code{panelPomp} or inheriting class
#' \code{panelPomp}.
#' @param start,end position in original \code{times(pomp)} at which to start.
#' @param i unit index (indices) or name (names).
#' @param value value to be assigned.
#' @param format the format (data type) of the return value.
#' @param ... ....
#' @section Methods:
#' \describe{
#'   \item{coef}{Extracts coefficients of \code{panelPomp} objects.}
#'   \item{coef<-}{Assign coefficients to \code{panelPomp} objects.}
#'   \item{length}{Count the number of units in \code{panelPomp} objects.}
#'   \item{names}{Get the unit names of \code{panelPomp} objects.}
#'   \item{pparams}{Extracts coefficients from \code{panelPomp} objects in list form.}
#'   \item{pParams}{Converts panel coefficients from vector form to list form.}
#'   \item{window}{Subset \code{panelPomp} objects by changing start time and
#'   end time.}
#'   \item{\code{[]}}{Take a subset of units.}
#'   \item{\code{[[]]}}{Select the pomp object for a single unit.}
#'   }
#' @author Carles \Breto, Aaron A. King, Edward L. Ionides, Jesse Wheeler
#' @family panelPomp methods
NULL

#' @rdname panelPomp_methods
#' @return
#' \code{coef()} returns a \code{numeric} vector.
#' @examples
#' ## access and manipulate model parameters and other features
#' prw <- panelRandomWalk()
#' coef(prw)
#' @export
setMethod(
  "coef",
  signature=signature(object="panelPomp"),
  definition = function (object) {
    pmat <- object@specific
    c(
      object@shared,
      setNames(
        as.numeric(pmat),
        outer(rownames(pmat),colnames(pmat),sprintf,fmt="%s[%s]")
      )
    )
  }
)

#' @rdname panelPomp_methods
#' @examples
#' # replace coefficients
#' coef(prw) <- c(sigmaX=2,coef(prw)[-1])
#' coef(prw)
#' @export
setMethod(
  "coef<-",
  signature=signature(object="panelPomp"),
  definition=function (object, ..., value) {
    ## check names(value)
    ep <- wQuotes("in ''coef<-'': ")
    if (is.list(value)) value <- unlist(value)
    if (!identical(character(0),setdiff(names(value),names(coef(object)))))
      stop(wQuotes(ep,"part of ''value'' is not part of ''coef(object)''","."),call.=FALSE)
    if (!identical(character(0),setdiff(names(coef(object)),names(value))))
    stop(wQuotes(ep,"part of ''coef(object)'' is not specified in ''value''","."),
         call.=FALSE)
    nn <- grep("^.+\\[.+?\\]$",names(value),perl=TRUE,value=TRUE)
    pp <- sub(pattern="^(.+?)\\[.+?\\]$",replacement="\\1",x=nn,perl=TRUE)
    uU <- names(object@unit_objects)
    pU <- sort(unique(pp))
    object@specific <- array(dim=c(length(pU),length(uU)),
                                     dimnames=list(param=pU,unit=uU))
    pvec <- setNames(numeric(length(object@specific)),
                     outer(pU,uU,sprintf,fmt="%s[%s]"))
    unitpar <- intersect(names(value),names(pvec))
    sharedpar <- setdiff(names(value),unitpar)
    pvec[unitpar] <- value[unitpar]
    object@specific[,] <- pvec
    object@shared <- value[sort(sharedpar)]
    validObject(object)
    object
  }
)

#' @rdname panelPomp_methods
#' @return
#' \code{length()} returns an \code{integer}.
#' @examples
#' length(prw)
#' @export
setMethod(
  "length",
  signature=signature(x="panelPomp"),
  definition = function (x) length(x@unit_objects)
)

#' @rdname panelPomp_methods
#' @return
#' \code{names()} returns a \code{character} vector.
#' @examples
#' names(prw)
#' @export
setMethod(
  "names",
  signature=signature(x="panelPomp"),
  definition = function (x) names(x@unit_objects)
)

#' @rdname panelPomp_methods
#' @return \pparamsReturn
# \pparamsReturn is resused in documentation of generic function introduced by the panelPomp package
#' @example examples/pparams.R
#' @export
setMethod(
  "pparams",
  signature=signature(object="panelPomp"),
  definition = function (object)
    list(shared=object@shared,specific=object@specific)
)

#' @rdname panelPomp_methods
#' @return
#' \code{pParams()} returns a \code{list} with the model parameters in list form.
#' @examples
#' # convert vector-form parameters to list-form parameters
#' pParams(coef(prw))
#' @export
pParams <- function (value) {

  ep <- wQuotes("in ''pParams'': ")
  if (!is.vector(value)) stop(ep, "input must be a vector.", call. = FALSE)

  nn <- grep("^.+\\[.+?\\]$", names(value), perl = TRUE, value = TRUE)
  shs <- names(value)[!names(value) %in% nn]
  sps <- unique(sub(pattern="^(.+?)\\[.+?\\]$",replacement="\\1",x=nn,perl=TRUE))
  # sps <- sort(unique(pp))
  uu <- sub(pattern="^(.+?)\\[(.+?)\\]$",replacement="\\2",x=nn,perl=TRUE)
  us <- sort(unique(uu))
  pParams <- list(shared=numeric(0),specific=array(numeric(0),dim=c(0,0)))
  if (length(shs)>0) pParams$shared <- value[shs]
  if (length(sps)>0) {
    pParams$specific <- array(dim=c(length(sps),length(us)),
                              dimnames=list(param=sps,unit=us))
    for (sp in sps) {
      pParams$specific[sp,us] <- value[paste0(sp,"[",us,"]")]
    }
  }
  pParams
}

#' @rdname panelPomp_methods
#' @export
#' @examples
#' ## summaries of objects
#' print(prw)
#' @export
setMethod(
  "print",
  signature=signature(x="panelPomp"),
  definition=function (x, ...) {
    cat(wQuotes("<object of class ''panelPomp''>\n"))
    invisible(x)
  }
)

#' @rdname panelPomp_methods
#' @examples
#' show(prw)
#' @export
setMethod(
  "show",
  signature=signature(object="panelPomp"),
  definition=function (object) {
    print(object)
    cat("panel of",length(object),ifelse(length(object)>1,"units","unit"),"\n")
    if (length(coef(object))>0) {
      cat("parameter(s):\n")
      print(pParams(coef(object)))
    } else {
      cat("parameter(s) unspecified\n");
    }
    cat(paste0("summary of first panel unit (\"",names(object)[1],"\"):","\n"))
    show(object[[1]])
    invisible(NULL)
  }
)

#' @rdname panelPomp_methods
#' @return \unitobjectsReturn
# \unitobjectsReturn is resused in documentation of generic function introduced by the panelPomp package
#' @example examples/unitobjects.R
#' @export
setMethod(
  "unit_objects",
  signature = signature(object = "panelPomp"),
  definition = function(object) {
    object@unit_objects
  }
)

#' @rdname panelPomp_methods
#' @return
#' \code{window()} returns a \code{panelPomp} object with adjusted times.
#' @examples
#' ## select windows of time
#' window(prw,start=2,end=4)
#' @export
setMethod(
  "window",
  signature=signature(x="panelPomp"),
  definition=function (x, start, end) {
    tm <- time(x[[1]],t0=FALSE)
    if (missing(start)) start <- tm[1]
    if (missing(end)) end <- tm[length(tm)]
    panelPomp(
      lapply(x@unit_objects,FUN=window,start=start,end=end),
      shared=x@shared,
      specific=x@specific
    )
  }
)

#' @rdname panelPomp_methods
#' @return
#' \code{`[`} returns a \code{panelPomp} object.
#' @examples
#' ## subsetting panelPomp objects
#' prw[1] # panelPomp of 1 unit (first unit of prw)
#' @export
setMethod(
  "[",
  signature=signature(x="panelPomp"),
  definition=function (x, i) {
    panelPomp(
      x@unit_objects[i],
      shared=x@shared,
      specific=x@specific[,i,drop=FALSE]
    )
  }
)

#' @rdname panelPomp_methods
#' @return
#' \code{`[[`} returns a \code{pomp} object.
#' @examples
#' prw[[2]] # pomp object corresponding to unit 2 of prw
#' @export
setMethod(
  "[[",
  signature=signature(x="panelPomp"),
  definition=function (x, i) {
    po <- x@unit_objects[[i]]
    coef(po) <- c(x@shared,setNames(x@specific[,i],rownames(x@specific)))
    po
  }
)


## COERCE METHODS
#' @title Coercing \code{panelPomp} objects as \code{list}, \code{pompList} or
#' \code{data.frame}
#' @description When coercing to a \code{data.frame}, it coerces a
#' \code{panelPomp} into a \code{data.frame}, assuming units share common
#' variable names.
## '@rdname' [either 'panelPomp_methods' or 'as'] doesn't seem to work with setAs()
#' @name as
#' @family panelPomp methods
#' @return
#' An object of class matching that specified in the second argument (\code{to=}).
#' @author Carles \Breto
setAs(from="panelPomp",to="data.frame",
      def = function (from) {
        x <- lapply(from@unit_objects,as,"data.frame")
        for (u in seq_along(x)) {
          x[[u]]$unit <- names(from@unit_objects)[[u]]
        }
        do.call(rbind,x)
      }
)

#' @name as
# '@rdname as' doesn't seem to work; if '@name as' is not repeated:
# Warning: Block must have a @name
# Either document an existing object or manually specify with @name
# [however, '@title' and '@family' don't change their values in first '@name as']
#' @description When coercing to a \code{list}, it extracts the
#' \code{unit_objects} slot of \code{panelPomp} objects and attaches
#' associated parameters.
# @author Carles \Breto, Edward L. Ionides
setAs(from="panelPomp",to="list",def = function (from) {
  plist <- from@unit_objects
  shared <- from@shared
  specific <- from@specific
  for(u in 1:length(plist)) {
    coef(plist[[u]]) <- c(shared,setNames(specific[,u],rownames(specific)))
  }
  plist
})

#' @name as
# '@rdname as' doesn't seem to work; if '@name as' is not repeated:
# Warning: Block must have a @name
# Either document an existing object or manually specify with @name
# [however, '@title' and '@family' don't change their values in first '@name as']
#' @description When coercing to a \code{pompList}, it extracts the
#' \code{unit_objects} slot of \code{panelPomp} objects and attaches
#' associated parameters, converting the resulting list to a \code{pompList} to
#' help the assignment of pomp methods.
# @author Edward L. Ionides
setAs(from="panelPomp",to="pompList",def = function (from) {
  plist <- as(from,"list")
  class(plist) <- "pompList"
  plist
})

#' @rdname panelPomp_methods
#' @return
#' \code{specific()} returns unit-specific parameters as a numeric matrix or
#'    vector
#' @examples
#' ## access and manipulate model parameters and other features
#' prw <- panelRandomWalk(U = 4)
#' specific(prw)
#' @author Jesse Wheeler
#' @export
setMethod(
  "specific",
  signature=signature(object="panelPomp"),
  definition = function (object, ..., format = c("matrix", "vector")) {
    out_type <- match.arg(format)
    if (out_type == 'matrix') {
      return(object@specific)
    } else {
      pmat <- object@specific
      return(
        setNames(
          as.numeric(pmat),
          outer(rownames(pmat),colnames(pmat),sprintf,fmt="%s[%s]")
        )
      )
    }
  }
)

# TODO: update example.
# TODO: Implement "shared()<-", use that in "coef" function?
#' @rdname panelPomp_methods
#' @examples
#' # replace unit-specific coefficients
#' specific(prw) <- c("sigmaX[rw1]"=2)
#' specific(prw)
#' @export
setMethod(
  "specific<-",
  signature=signature(object="panelPomp"),
  definition=function (object, value) {
    ## check names(value)
    ep <- wQuotes("in ''specific<-'': ")

    if (is.matrix(value)) {
      sp_names <- rownames(object@specific)
      all_names <- c(names(object@shared), sp_names)
      uU <- names(object@unit_objects)

      # TODO: Make similar check in vector case
      # Has column that doesn't correspond to any unit-object
      if (!identical(character(0), setdiff(colnames(value), uU)))
        stop(wQuotes(ep, "''value'' contains unit names not in ''object''", "."), call. = FALSE)

      # Has parameter (row) that isn't part of the object (shared or unit specific)
      if (!identical(character(0), setdiff(rownames(value), all_names)))
        stop(wQuotes(ep, "''value'' contains parameters not found in ''object''", "."), call. = FALSE)

      # TODO: What I want this function to do is the following:
      #   - Allow specification of a few unit-specific parameters only.
      #     - If an existing unit-specific parameter is not specified, the
      #       value is kept at the original value.
      #     - If an existing unit-specific parameter is specified, the value
      #       is over-written.
      #     - If a shared parameter is specified, then change the value from
      #       shared -> specific.
      #     - If a non-existent parameter is specified, throw an error (this
      #       could be either because the unit doesn't exist, or the parameter
      #       doesn't exist.) (DONE!)

      missing_params <- setdiff(sp_names, rownames(value))
      missing_units <- setdiff(uU, colnames(value))
      shared2sp <- setdiff(rownames(value), sp_names)

      if (!identical(character(0), missing_units) | !identical(character(0), missing_params)) {
        # If the input "value" is missing either a unit or a parameter that is
        # contained in the original, then we will update value to contain these
        # missing parameters.

        # Create place-holder matrix for new unit-specific matrix. Will have the
        # same number of rows as "value" + the number of missing parameters that
        # will be retained from original object. Number of columns will match
        # number of units.
        tmp_value <- matrix(
          nrow = nrow(value) + length(missing_params),
          ncol = length(object@unit_objects)
        )

        # Name the dimensions of the placeholder matrix.
        dimnames(tmp_value) <- list(
          param = c(rownames(value), missing_params),
          unit = names(object@unit_objects)
        )

        # TODO: CHECK: Does this work if I change shared -> specific (i.e., add extra specific params)?
        # Does this work if "value" is a subset of "object@specific"?

        # Set default values to original values. We do not drop unit-specific
        # parameters in this method.
        tmp_value[rownames(object@specific), colnames(object@specific)] <- object@specific

        # Find non-missing values in "value". These are used to replace existing
        # default values.
        non_missing <- which(!is.na(value), arr.ind = TRUE)
        rows_to_replace <- rownames(value)[non_missing[, 1]]
        cols_to_replace <- colnames(value)[non_missing[, 2]]

        # Replace values in place-holder with updated values contained in
        # "value" object.
        tmp_value[cbind(rows_to_replace, cols_to_replace)] <- value[non_missing]
        value <- tmp_value
      }

      if (!identical(character(0), shared2sp)) {
        orig_shared <- object@shared[names(object@shared) %in% shared2sp]
        value[shared2sp, is.na(value[shared2sp, ])] <- orig_shared
        object@shared <- object@shared[!names(object@shared) %in% shared2sp]
      }

      object@specific <- value

      validObject(object)
      return(object)
    } else if (is.numeric(value)) {

      if (any(!grepl("^.+\\[.+\\]$",names(value))))
        stop(wQuotes(ep, "names of ''value'' must end in ''[unit_name]''", "."), call. = FALSE)

      nn <- grep("^.+\\[.+?\\]$",names(value),perl=TRUE,value=TRUE)
      pp <- sub(pattern="^(.+?)\\[.+?\\]$",replacement="\\1",x=nn,perl=TRUE)
      pU <- sort(unique(pp))
      value_units <- sub(pattern="^.+\\[(.+?)\\]$",replacement="\\1",x=nn,perl=TRUE)

      sp_names <- rownames(object@specific)
      sh_names <- names(object@shared)
      all_names <- c(sh_names, sp_names)
      uU <- names(object@unit_objects)

      if (!identical(character(0), setdiff(value_units, uU)))
        stop(wQuotes(ep, "''value'' contains unit names not in ''object''", "."), call. = FALSE)

      # Trying to add a new parameter that isn't part of existing object
      if (!identical(character(0),setdiff(pU,all_names)))
        stop(wQuotes(ep,"''value'' contains parameters not found in ''object''","."),call.=FALSE)

      # Missing a unit-specific parameter that is part of existing object
      if (!identical(character(0),setdiff(sp_names,names(value)))) {
        # Get all of the existing unit-specific parameters
        nn_old <- grep("^.+\\[.+?\\]$",names(coef(object)),perl=TRUE,value=TRUE)

        # Add old existing parameters to vector of new values
        value <- c(value, coef(object)[setdiff(nn_old,names(value))])
        new_order <- sort(names(value))
        value <- value[new_order]
      }

      if (any(pU %in% sh_names)) {
        orig_shared <- object@shared[pU]
        new_uu <- setNames(
          rep(orig_shared, each = length(uU)),
          paste0(rep(names(orig_shared), each = length(uU)), rep(paste0('[', uU, ']'), length(orig_shared)))
        )
        value <- c(value, new_uu[setdiff(names(new_uu), names(value))])
        new_order <- sort(names(value))
        value <- value[new_order]
      }

      value <- c(object@shared[setdiff(sh_names, pU)], value)

      nn <- grep("^.+\\[.+?\\]$",names(value),perl=TRUE,value=TRUE)
      pp <- sub(pattern="^(.+?)\\[.+?\\]$",replacement="\\1",x=nn,perl=TRUE)
      uU <- names(object@unit_objects)
      pU <- sort(unique(pp))
      object@specific <- array(dim=c(length(pU),length(uU)),
                               dimnames=list(param=pU,unit=uU))
      pvec <- setNames(numeric(length(object@specific)),
                       outer(pU,uU,sprintf,fmt="%s[%s]"))
      unitpar <- intersect(names(value),names(pvec))
      sharedpar <- setdiff(names(value),unitpar)
      pvec[unitpar] <- value[unitpar]
      object@specific[,] <- pvec
      object@shared <- value[sort(sharedpar)]
      validObject(object)
      object
    }
  }
)

#' @rdname panelPomp_methods
#' @return
#' \code{shared()} returns shared parameters from a panelPomp object
#' @examples
#' ## access and manipulate model parameters and other features
#' prw <- panelRandomWalk(U = 4)
#' shared(prw)
#' @author Jesse Wheeler
#' @export
setMethod(
  "shared",
  signature=signature(object="panelPomp"),
  definition = function (object) {
    object@shared
  }
)

#' @rdname panelPomp_methods
#' @examples
#' prw <- panelRandomWalk(U = 4)
#' # replace unit-specific coefficients
#' shared(prw) <- c(sigmaX=2)
#' shared(prw)
#' @export
setMethod(
  "shared<-",
  signature=signature(object="panelPomp"),
  definition=function (object, value) {
    ## check names(value)
    ep <- wQuotes("in ''shared<-'': ")

    sp_names <- rownames(object@specific)
    sh_names <- names(object@shared)
    all_names <- c(sh_names, sp_names)

    # Trying to add a new parameter that isn't part of existing object
    if (!identical(character(0),setdiff(names(value),all_names)))
      stop(wQuotes(ep,"''value'' contains parameters not found in ''object''","."),call.=FALSE)

    if (!identical(character(0), setdiff(sh_names, names(value)))) {
      value <- c(value, object@shared[setdiff(sh_names, names(value))])
    }

    if (any(names(value) %in% sp_names)) {
      object@specific <- object@specific[setdiff(sp_names, names(value)), ]
    }

    object@shared <- value
    validObject(object)
    object
  }
)
