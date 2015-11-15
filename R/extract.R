# not work for deep levels

depth <- function(l) {
  if(!is.list(l)) {
    return(0)
  } else {
    return(max(unlist(lapply(l, depth))) + 1)
  }
}

is_select <- function(l, by) {
  is_select <- rep(TRUE, length(l[[1]]))
  if (sum(names(by) %in% names(l)) == 0) {
    return(is_select)
  } else {
    for (names_arg in names(by)) {
      if (names_arg %in% names(l)) {
        is_select <- (is_select & (l[[names_arg]] %in% by[[names_arg]]))
      }
    }
    return(is_select)
  }
}

extract_1_level <- function(l, is.select) {
  # be sure l is a list
  return(lapply(l, function(x) x[is.select]))
}


# add parameter depth?
extract_2 <- function(l, by) {
  is.select <- Reduce(`&`, lapply(l, is_select, by = by))
  lapply(l, extract_1_level, is.select = is.select)
}


extract <- function(l, by) {
  # be sure by is a list
  # list not matrix ?
  if (depth(l) == 0) {
    return(l)
  }
  if (depth(l) == 1 && length(by) > 0) {
    return(extract_1_level(l = l, is_select(l, by = by)))
  } else if (depth(l) > 1 && length(by) > 0) {
    names_reduced <- setdiff(names(by), names(l))
    if (length(names_reduced) == 0) {
      return(extract_1_level(l = l, by = by))
    } else {
      l <- extract_1_level(l = l, by = by)
      # search name_reduced
      # l[[1]] no longer list
      # lapply can't broad the slices
      # can not use recursive? use while?
      return(lapply(l, extract, by = by[names_reduced]))
    }

  }
}

