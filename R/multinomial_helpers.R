get_levels <- function(x) {
  if (is.factor(x)) {
    return(levels(x))
  } else {
    return(sort(unique(x)))
  }
}

pack_predictions <- function(pred_matrix) {
  packed <- apply(pred_matrix, 1, function(row) {
    packed_row <- list(row)
    class(packed_row) <- "packed_predictions"
    return(packed_row)
  })
  return(as.matrix(packed))
}

unpack_predictions <- function(x) {
  do.call(rbind, lapply(x, `[[`, 1))
}

print.packed_predictions <- function(x) {
  print(unlist(x))
}

normalize_rows <- function(x) {
  sweep(x, 1, rowSums(x), "/")
}
