cv <- function(.x, .fun, ..., folder = 5) {
  nobs <- nrow(.x)
  index <- sample(seq(folder), nobs, replace = TRUE)
}