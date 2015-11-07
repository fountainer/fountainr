make_header <- function() {
  num_hash_3 <- 0L
  num_hash_4 <- 0L
  num_hash_5 <- 0L
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
      str_result <- paste(paste0(rep("#", n), collapse = ""), paste0(num_hash_3, ".", num_hash_4, ".", num_hash_5), " ")
    } else {
      str_result <- "3 - 5 of # are required."
    }
    return(str_result)
  }
  return(header)
}