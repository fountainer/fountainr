#' Write all objects in the global environment to a specified file
#'
#' 
#' @param file The name of the file that the objects are written to.
#' @export
#' 
#' @examples
#' a <- 1
#' save_all("test.RData")
save_all <- function(file = stop("'file' must be specified")) {
  global_objects <- lapply(ls(globalenv()), as.name)
  param.list <- c(global_objects, list(list = character(), file = file))
  do.call(save, param.list)
}