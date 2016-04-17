#' Rolling apply a function on a data frame or matrix.
#' 
#' @param data      A data frame or a matrix.
#' @param width     The rolling windown size.
#' @param FUN       function whose first parameter is the \code{data}.
#' @param ...       Additional parameter passed to \code{FUN}
#' @param parallel  parallel computing. Default is FALSE. If parallel is positive, 
#' parallel is the number of cores. If parallel is negative, the number of cores is \code{detectCores() + parallel}
#' @param .export   character vector of variables to export. Default is NULL.
#' @param .packages character vector of packages that tasks depends on.
#' @param progress  Create a progress bar. Default FALSE.
#' @return  A list of result.
#' @export
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom parallel detectCores clusterExport makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom tcltk tkProgressBar setTkProgressBar
#' @examples
#' res <- roll_apply_par(matrix(seq(10^4 * 3), ncol = 3), 100, function(m) eigen(cov(m))$values, parallel = TRUE, progress = TRUE)
roll_apply_par <- function(data, width, FUN, ..., parallel = FALSE, .export = NULL, .packages = NULL, progress = FALSE) {
  FUN <- match.fun(FUN)
  nobs <- nrow(data)
  if (nobs < width) {
    stop("The size of window is larger than row number of the data.")
  }
  num_wins <- nobs - width + 1
  vec <- seq(num_wins)

  if (parallel) {
    if (isTRUE(parallel)) {
      cores <- detectCores()
    } else {
      if (parallel > 0) {
        cores <- parallel
      } else if (abs(parallel) < detectCores()) {
        cores <- detectCores() + parallel
      } else {
        stop("The reduced number is larger than the number of the detected cores.")
      }
    }
    cl <- makeCluster(cores)
    clusterExport(cl, .export)
    registerDoParallel(cl)
    `%fun%` <- `%dopar%`
  } else {
    `%fun%` <- `%do%`
  }
  if (progress) {
    .packages <- c(.packages, "tcltk")
    res <- foreach(i = vec, .packages = .packages) %fun% {
      if (!exists("pb")) {
        pb <- tkProgressBar(min = 1, max = num_wins)
      }
      setTkProgressBar(pb, i, title = paste(round(100 * i / num_wins, 2), "% done!"))
      FUN(data[i:(i + width - 1), ], ...)
    }
  } else {
    res <- foreach(i = vec, .packages = .packages) %fun% FUN(data[i:(i + width - 1), ], ...)
  }
  

  if (parallel) {
    stopCluster(cl)
  }
  return(res)
} 