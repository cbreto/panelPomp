#' @include panelPomp-package.R
NULL

## change the "pomp.examples' global option when the "panelPomp" package is loaded/unloaded
.onAttach <- function (...) {
  exampleDir <- getOption("pomp.examples")
  newDir <- system.file("examples",package="panelPomp")
  options(pomp.examples=c(exampleDir,newDir,recursive=TRUE))
}

.onDetach <- function (...) {
  exampleDir <- getOption("pomp.examples")
  newDir <- system.file("examples",package="panelPomp")
  exampleDir <- exampleDir[exampleDir!=newDir]
  options(pomp.examples=exampleDir)
}

## Go to list-form pparams from matrix specification
toListPparams <- function(
  matrixPparams,
  names.in.vector,
  vector.position.in.listPparams,
  vector.name.in.listPparams,
  matrix.name.in.listPparams
  ) {
  output <- as.vector(c(NA,NA), mode = "list")
  # fill vector in
  output[[vector.position.in.listPparams]] <- matrixPparams[names.in.vector,1]
  # fill matrix in
  name.logicals.for.matrix <- !rownames(matrixPparams) %in% names.in.vector
  output[[ifelse(vector.position.in.listPparams==1,2,1)]] <-
    if(any(name.logicals.for.matrix)) {
      matrixPparams[name.logicals.for.matrix, , drop = FALSE]} else {
        # fill empty matrix in
        array(
          data = numeric(0), 
          dim = c(0, dim(matrixPparams)[2]),
          dimnames = list(NULL, dimnames(matrixPparams)[[2]])
          )
      }
  names(output)[vector.position.in.listPparams] <- vector.name.in.listPparams
  names(output)[ifelse(vector.position.in.listPparams==1,2,1)] <- matrix.name.in.listPparams
  output
  }


## Go to matrix-form pparams from list specification
toMatrixPparams <- function(listPparams) {
  common.params <- listPparams[[which(sapply(listPparams, is.vector))]]
  specific.params <- listPparams[[which(sapply(listPparams, is.matrix))]]
  
  U <- dim(specific.params)[2]
  matrixPparams <- rbind(
    matrix(
      rep(x = common.params, times = U),
      ncol = U,
      dimnames = list(names(common.params), NULL)
    ),
  specific.params)
  matrixPparams
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
  }# END FN runif.EstimationScale
