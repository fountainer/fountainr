#' Automatically number headers in markdown file
#'
#' Returns a function that can automatically number headers. Only support header 3, 4, 5.
#'
#' @param n The number of the first header 3.Default 1.
#'
#' @return A function with a integer parameter(3, 4, 5, 6) that can automatically number headers
#' @export
#'
#' @examples
#' h <- make_header()
#' # write in rmarkdown
#' `r h(3)` H3
#'
#' `r h(4)` H4
make_header <- function(n = 1) {
  num_hash_3 <- n - 1
  num_hash_4 <- 0L
  num_hash_5 <- 0L
  num_hash_6 <- 0L
  header <- function(n) {
    if (n == 3) {
      num_hash_3 <<- num_hash_3 + 1
      num_hash_4 <<- 0L
      num_hash_5 <<- 0L
      str_result <- paste(paste0(rep("#", n), collapse = ""), num_hash_3, " ")
    } else if (n == 4) {
      num_hash_4 <<- num_hash_4 + 1
      num_hash_5 <<- 0L
      str_result <- paste(paste0(rep("#", n), collapse = ""), paste0(num_hash_3, ".", num_hash_4), " ")
    } else if (n == 5) {
      num_hash_5 <<- num_hash_5 + 1
      num_hash_6 <<- 0L
      str_result <- paste(paste0(rep("#", n), collapse = ""), paste0(num_hash_3, ".", num_hash_4, ".", num_hash_5), " ")
    } else if (n == 6) {
      num_hash_6 <<- num_hash_6 + 1
      str_result <- paste(paste0(rep("#", n), collapse = ""), paste0(num_hash_3, ".", num_hash_4, ".", num_hash_5, ".", num_hash_6), " ")
    } else {
      str_result <- "3 - 6 is required."
    }
    return(str_result)
  }
  return(header)
}