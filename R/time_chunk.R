#' Time the knitr chunk.
#' 
#' @return A hook function
#' @export
#' 
#' @examples
#' knit_hooks$set(
#'   timeit = time_chunk()
#' )
time_chunk <- function() {
  now = NULL
  timeit <- function(before) {
    if (before) { 
      now <<- Sys.time() 
    } else {
      time_diff <- Sys.time() - now
      paste(round(time_diff, digits = 2), units(time_diff), " (", now, ",", Sys.time(), ")")
    }
  }
  return(timeit)
}