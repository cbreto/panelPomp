#' @include generics.R
NULL

#' @title Get single column or row without dropping names
#' @description Subset matrix dropping dimension but without dropping dimname
#' (as done by \code{`[`} by default).
#' @param matrix matrix.
#' @param row integer; single row to subset.
#' @param rows numeric; rows to subset; like with \code{`[`}, this argument can be left empty to designate all rows.
#' @param col integer; single column to subset.
#' @param cols numeric; columns to subset; like with \code{`[`}, this argument can be left empty to designate all columns.
#' @return
#' A named \code{vector} object.
#' @name get_dim
NULL

#' @rdname get_dim
#' @examples
#' m <- matrix(NA,dimnames=list('r1','c1'))
#' m[1,1] # = NA; R removes both names
#' get_col(m,rows=1,col=1) # = c(r1=NA) keeps colname
#' @export
#' @author Carles \Breto
get_col <- function (matrix, rows, col) {
  stopifnot(is.matrix(matrix)&!missing(col))
  # if all rows, i.e., get.col(mtrx,,cl)
  if(all('rows'!=names(as.list(match.call())))) {
    matrix[,col]
  } else {
    setNames(matrix[rows,col],nm=rownames(matrix)[rows]
    )
  }
}

#' @rdname get_dim
#' @examples
# m <- matrix(NA,dimnames=list('r1','c1'))
# m[1,1] # = NA
#' get_row(m,row=1,cols=1) # = c(c1=NA) keeps rowname
#' @export
# @author Carles \Breto
get_row <- function (matrix, row, cols) {
  stopifnot(is.matrix(matrix)&!missing(row))
  # if all columns, i.e., get.col(mtrx,rw,)
  if(all('cols'!=names(as.list(match.call())))) {
    matrix[row,]
  } else {
    setNames(matrix[row,cols],nm=colnames(matrix)[cols]
    )
  }
}
