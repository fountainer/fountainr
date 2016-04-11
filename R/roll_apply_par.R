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
#' @return  A list of result.
#' @export
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom parallel detectCores clusterExport makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @examples
#' res <- roll_apply_par(seq(10^3), 100, sqrt, parallel = TRUE)
roll_apply_par <- function(data, width, FUN, ..., parallel = FALSE, .export = NULL, .packages = NULL) {
  FUN <- match.fun(FUN)
  nobs <- nrow(data)
  if (nobs < width) {
    stop("The size of window is larger than row number of the data.")
  }
  vec <- seq(nobs - width + 1)

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

  res <- foreach(i = vec, .packages = .packages) %fun% FUN(data[i:(i + width - 1), ], ...)

  if (parallel) {
    stopCluster(cl)
  }
  return(res)
} 