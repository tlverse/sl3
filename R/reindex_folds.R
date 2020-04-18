reindex <- function(index, subset) {
  matches <- match(index, subset)
  reindexed <- matches[!is.na(matches)]

  return(reindexed)
}

subset_fold <- function(fold, subset) {
  origami::make_fold(
    fold_index(),
    reindex(training(), subset),
    reindex(validation(), subset)
  )
}

#' Make folds work on subset of data
#'
#' subset_folds takes a origami style folds list, and returns a
#' list of folds applicable to a subset, by subsetting the training and validation index vectors
#'
#' @param folds an origami style folds list
#' @param subset an index vector to be used to subset the data
#' @export
subset_folds <- function(folds, subset) {
  if(is.null(folds)){
    return(NULL)
  }
  subsetted <- lapply(folds, subset_fold, subset)

  return(subsetted)
}
