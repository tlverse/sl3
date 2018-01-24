get_levels <- function(x) {
  if (is.factor(x)) {
    return(levels(x))
  } else {
    return(sort(unique(x)))
  }
}

#' Pack multidimensional predictions into a vector (and unpack again)
#' @rdname pack_predictions
#' @param pred_matrix a matrix of prediciton values
#' @export
pack_predictions <- function(pred_matrix) {
  packed <- apply(pred_matrix, 1, function(row) {
    packed_row <- list(row)
    class(packed_row) <- "packed_predictions"
    return(packed_row)
  })
  return(as.matrix(packed))
}

#' @rdname pack_predictions
#' @param x a packed prediction list
#' @export
unpack_predictions <- function(x) {
  do.call(rbind, lapply(x, `[[`, 1))
}

print.packed_predictions <- function(x) {
  print(unlist(x))
}

normalize_rows <- function(x) {
  sweep(x, 1, rowSums(x), "/")
}


#' Convert Factors to indicators
#' 
#' replicates the functionality of model.matrix, but faster
#' 
#' @param x the factor to expand
#' @param ind_ref_mat a matrix used for expansion, if NULL generated automatically
#' @rdname factors_to_indicators
#' @export
factor_to_indicators <- function(x, ind_ref_mat=NULL) {
  x_vals <- sl3:::get_levels(x)
  if(is.null(ind_ref_mat)){
    ind_ref_mat <- sapply(x_vals[-1], function(x_val) as.numeric(x_val == x_vals))
  }
  
  ind_mat <- ind_ref_mat[as.numeric(x),]
  return(ind_mat)
}

#' Convert Factors to indicators
#' 
#' replicates the functionality of model.matrix, but faster
#' 
#' @param dt the dt to expand
#' @rdname factors_to_indicators
#' @export
dt_expand_factors <- function(dt){
  raw = lapply(dt,function(dt_col){
    if(is.factor(dt_col)){
      return(factor_to_indicators(dt_col))
    } else {
      return(dt_col)
    }
    
  })
  as.data.table(raw)
}
