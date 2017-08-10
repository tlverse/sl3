loss_squared_error <- function(truth, pred) {
    (pred - truth)^2
}

#' @importFrom stats weighted.mean
risk <- function(truth, pred, loss = loss_squared_error, weights = NULL) {
    if (is.null(weights)) {
        weights <- rep(1, length(truth))
    }
    
    risk <- weighted.mean(loss(truth, pred), weights)
}
