#' A wrapper of sample
#'
#' If \code{replace = TRUE} and \code{all = TRUE}, sampling \code{length(x)} of items without replacement first,
#' then sampling with replacement
#'
#' @param x Either a vector of one or more elements from which to choose, or a positive integer
#' @param size a non-negative integer giving the number of  items to choose
#' @param replace Should sampling be with replacement?
#' @param prob A vector of probability for obtaining the elements of the vector being sampled
#' @param all If \code{replace = TRUE} and \code{all = TRUE}, sampling \code{length(x)} of items without replacement first, then sampling with replacement
#'
#' @return For sample a vector of length size with elements drawn from either x or from the integers 1:x.
#' @export
#'
#' @examples sample_all(1:5, 6)
sample_all <- function(x, size, replace = TRUE, prob = NULL, all = TRUE) {
  if (replace && all) {
    if (length(x) <= size) {
      res_no_replacement <- sample(x, length(x), replace = FALSE, prob = prob)
      res_replacement <- sample(x, size - length(x), replace = TRUE, prob = prob)
      res <- c(res_no_replacement, res_replacement)
      # Is it necessary to sample res again?
      return(sample(res, length(res), replace = FALSE, prob = prob))
    } else {
      stop("length(x) > size")
    }
  } else {
    return(sample(x, size, replace = replace, prob = prob))
  }
}