#' Convert a list to a data frame.
#'
#' Convert a list to a data frame. Just a wrapper of rbindlist in data.table
#' @param l         A list constaining data.table, data.frame, matrix or list object or vector.
#' @param use.names If TRUE items will be bound by matching column names.
#' @param fill      If TRUE fills missing columns with NAs.
#' @param idcol     Generates an index column. Default (NULL) is not to.
#' @return  A data frame.
#' @export
#' 
#' @examples
#' df <- list2df(list(w1 = list(a = 1, b = 2, c = 3), w2 = list(a = 4, b = 5, c = 6)))
list2df <- function(l, use.names = fill, fill = FALSE, idcol = NULL) {
  # deal with list that contains vectors
  to_list_df <- function(m) {
    if (is.atomic(m)) {
      res <- as.list(m)
    } else if (is.matrix(m)) {
      res <- as.data.frame(m)
    } else {
      res <- m
    }
    return(res)
  }
  l <- lapply(l, to_list_df)
  DT <- data.table::rbindlist(l, use.names = use.names, fill = fill, idcol = idcol)
  data.table::setDF(DT)
  return(DT)
}