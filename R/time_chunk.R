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
      paste("Chunk rendering time:", round(Sys.time() - now, digits = 3))
    }
  }
  return(timeit)
}