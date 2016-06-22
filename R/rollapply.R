#' Rolling apply a function on a data frame or matrix.
#' 
#' @param data      A data frame or a matrix.
#' @param width     The rolling window size.
#' @param FUN       function whose first parameter is the \code{data}.
#' @param ...       Additional parameter passed to \code{FUN}
#' @param parallel  parallel computing. Default is FALSE. If parallel is positive, 
#' parallel is the number of cores. If parallel is negative, the number of cores is \code{detectCores() + parallel}
#' @param .export   character vector of variables to export. Default is NULL.
#' @param .packages character vector of packages that tasks depends on.
#' @param progress  The name of the file that is written into progress information. Default FALSE.
#' @return  A list of result.
#' @export
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom parallel detectCores clusterExport makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @examples
#' res <- roll_apply_par(matrix(seq(10^4 * 3), ncol = 3), 100, function(m) eigen(cov(m))$values, parallel = -1, progress = "progress.txt")
roll_apply <- function(data, width, FUN, ..., parallel = FALSE, .export = NULL, .packages = NULL, progress = FALSE) {
  FUN <- match.fun(FUN)
  nobs <- nrow(data)
  if (nobs < width) {
    stop("The size of window is larger than row number of the data.")
  }
  num_wins <- nobs - width + 1
  vec <- seq(num_wins)
  progress_messages <- function(i, start_time) {
    diff_time <- Sys.time() - start_time
    time_unit <- units(diff_time)
    perc_done <- round(i * 100 / num_wins, 2)
    perc_todo <- 100 - perc_done
    messages <- paste0(perc_done, "% done!")
    messages <- paste(messages, diff_time, time_unit, "used! Expected", perc_todo / perc_done * diff_time, time_unit, "left")
    return(messages)
  }
  if (class(progress) == "character") {
    if (!file.exists("progress")) {
      dir.create("progress")
    }
    progress <- paste0("progress/", progress)
    if (file.exists(progress)) {
      file.remove(progress)
    }
  }
  # ugly structure
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
    if (class(progress) == "character") {
      cl <- makeCluster(cores, outfile = file.path(progress))
    } else {
      cl <- makeCluster(cores)
    }
    # .export does not work?
    clusterExport(cl, .export)
    registerDoParallel(cl)
    
    if (class(progress) == "character") {
      start_time <- Sys.time()
      res <- foreach(i = vec, .packages = .packages) %dopar% {
        FUN(data[i:(i + width - 1), ], ...)
        messages <- progress_messages(i, start_time)
        writeLines(messages)
      }
    } else {
      res <- foreach(i = vec, .packages = .packages) %dopar% FUN(data[i:(i + width - 1), ], ...)
    }

  } else {
    if (class(progress) == "character") {
      con <- file(progress, "w+")
      start_time <- Sys.time()
      res <- foreach(i = vec, .packages = .packages) %do% {
        FUN(data[i:(i + width - 1), ], ...)
        messages <- progress_messages(i, start_time)
        writeLines(messages, con)
      }
      close(con)
    } else {
      res <- foreach(i = vec, .packages = .packages) %do% FUN(data[i:(i + width - 1), ], ...)
    }
  }

  if (parallel) {
    stopCluster(cl)
  } 
  return(res)
}



