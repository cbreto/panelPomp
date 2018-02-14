#' @include generics.R
NULL

#' @title Get single column or row without dropping names
#' @description Subset matrix dropping dimension but without dropping dimname 
#' (which is R's default).
#' @param matrix matrix.
#' @param row numeric; single row to subset.
#' @param rows numeric; rows to subset; like with `[`, this argument can be left empty to designate all rows.
#' @param col numeric; single column to subset.
#' @param cols numeric; columns to subset; like with `[`, this argument can be left empty to designate all columns.
#' @name get_dim
NULL

#' @rdname get_dim
#' @export
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
#' @export
#'
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
