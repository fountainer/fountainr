#' Shutdown the computer.
#'
#' @param times The times to shutdown the computer. Unit is second.
#' @export
#' @return The time when the function is called
#' 
#' @examples shutdown(10)
shutdown <- function(times) {
  system(paste("shutdown -s -t", times))
  return(Sys.time())
}