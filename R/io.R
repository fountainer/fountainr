
#' Write data to a CSV file.
#'
#' Write data to a CSV file. The name of CSV file is the same with the name of the data.
#'
#' @param x the object to be written, preferably a Vector, matrix, or data frame
#'
#' @export
write_table <- function(x) {
  file_name <- deparse(substitute(x))
  write.table(x, paste0(file_name, ".csv"), sep = ",", row.names = FALSE, quote = FALSE)
}