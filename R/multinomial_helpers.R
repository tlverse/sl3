get_levels <- function(x){
  if(is.factor(x)){
    return(levels(x))
  } else {
    return(sort(unique(x)))
  }
}

pack_predictions <- function(pred_matrix){
  packed <- apply(pred_matrix,1,function(row){
    packed_row <- list(row)
    class(packed_row) <- "packed_predictions"
    return(packed_row)
  })
  
  return(as.matrix(packed))
}

unpack_predictions <- function(x){
  do.call(rbind,lapply(x,`[[`,1))
}

print.packed_predictions <- function(x){
  print(unlist(x))
}

normalize_rows <- function(x) {
  sweep(x, 1, rowSums(x), "/")
}

mn_loglik <- function(pred, truth) {
  #make index matrix
  index_mat <- cbind(seq_along(truth), truth)
  unpacked <- unpack_predictions(pred)
  class_liks <- log(unpacked[index_mat])
  return(-1 * class_liks)
}

mn_logit <- function(alpha, x) {
  
  unpacked <- lapply(as.data.frame(x),unpack_predictions)
  multiplied <- mapply(`*`, unpacked, alpha, SIMPLIFY = FALSE)
  Y_pred <- Reduce(`+`, multiplied)
  
  # normalize so class predictions sum to 1
  Y_pred <- normalize_rows(Y_pred)
  
  return(pack_predictions(Y_pred))
}
