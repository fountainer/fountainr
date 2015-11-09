#' Rolling apply a function on a data frame or matrix.
#'
#' @param x A data frame or a matrix.
#' @param n The rolling windown size.
#' @param fun A function whose first parameter is the \code{x}.
#' @param ... Additional parameter passed to \code{fun}
#'
#' @return A list containing the rolling results
#' @export
#'
#' @examples
#' m <- matrix(1:10, ncol = 2)
#' roll_apply(m, 4, colMeans)
roll_apply <- function(x, n, fun, ...) {
  fun <- match.fun(fun)
  n_row <- nrow(x)
  if (n_row < n ) {
    stop("The size of window is larger than row number of the data.")
  }
  vec_seq <- seq_len(n_row - n + 1)
  names(vec_seq) <- paste0("win", vec_seq)
  return(lapply(vec_seq, function(i) fun(x[i:(i + n - 1), ], ...)))
}