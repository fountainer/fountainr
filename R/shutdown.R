#' Shutdown the computer.
#'
#' @param times The times to shutdown the computer. Unit is second.
#' @export
#' 
#' @examples shutdown(10)
shutdown <- function(times) {
  system(paste0("shutdown -s -t", times))
}