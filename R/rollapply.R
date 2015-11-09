#' Rolling apply a function on a data frame or matrix.
#'
#' @param x A data frame or a matrix.
#' @param n The rolling windown size.
#' @param fun A function whose first parameter is the \code{x}.
#' @param ... Additional parameter passed to \code{fun}
#' @param SIMPLIFY Attempt to return a data frame if \code{fun} returns a vector or a list whose elements are vectors with length 1.
#'
#'
#' @return A data frame or list containing the rolling results. See \code{SIMPLIFY}
#' @export
#'
#' @examples
#' m <- matrix(1:10, ncol = 2)
#' roll_apply(m, 4, colMeans)
roll_apply <- function(x, n, fun, ..., SIMPLIFY = TRUE) {
  fun <- match.fun(fun)
  n_row <- nrow(x)
  if (n_row < n ) {
    stop("The size of window is larger than row number of the data.")
  }
  vec_seq <- seq_len(n_row - n + 1)
  names(vec_seq) <- paste0("win", vec_seq)
  res <- lapply(vec_seq, function(i) fun(x[i:(i + n - 1), ], ...))
  if (SIMPLIFY) {
    if (all(unlist(lapply(res[[1]], is.atomic)) == TRUE) &&
        all(unlist(lapply(res[[1]], function(x) length(x) == 1)) == TRUE)) {
      # TODO: If fun returns list with different types...
      res <- as.data.frame(t(as.data.frame(lapply(res, unlist))))
    }
  }
  return(res)
}