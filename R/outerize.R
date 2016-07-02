#' Outerize a Scalar Function
#'
#' Create a function wrapper that outerizes the action of its argument \code{FUN}.
#' 
#' \code{outerize} is just function that calls \code{\link[base]{Vectorize}} recursively. 
#' Like \code{\link{Vectorize}}, it can not be used with primitive functions and 
#' the functions that have arguments named \code{FUN}, \code{vectorize.args}, 
#' \code{SIMPLIFY}, \code{USE.NAMES}.
#' @param FUN           function to apply, found via \code{\link[base]{match.fun}}
#' @param outerize.args A character vector of arguments which should be outerized.
#' @return      A function with the same arguments as \code{FUN}.
#' @export
#' @examples
#' f <- function(x, y) x + y
#' f_outerized <- outerize(f, c("x", "y"))
#' f_outerized(c(1, 2), c(3, 4, 5))
outerize <- function(FUN, outerize.args) {
  if (length(outerize.args) > 1) {
    args.last <- outerize.args[length(outerize.args)]
    FUN <- Vectorize(FUN, vectorize.args = args.last, SIMPLIFY = FALSE)
    outerize.args <- setdiff(outerize.args, args.last)
    return(outerize(FUN, outerize.args))
  } else if (length(outerize.args) == 1){
    return(Vectorize(FUN, outerize.args, SIMPLIFY = FALSE))
  } else {
    stop("length of outerize.args must be positive.")
  }
}