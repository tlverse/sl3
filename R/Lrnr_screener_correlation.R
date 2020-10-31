#' Correlation Screening Procedures
#'
#' This learner provides covariate screening procedures by running a test of
#' correlation (Pearson default) with the \code{\link[stats]{cor.test}}
#' function, and then selecting the (1) top ranked variables (default), or (2)
#' the variables with a pvalue lower than some pre-specified threshold.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#'
#' @export
#'
#' @keywords data
#'
#' @return Learner object with methods for training and prediction. See
#'  \code{\link{Lrnr_base}} for documentation on learners.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @family Learners
#'
#' @section Parameters:
#' \describe{
#'   \item{\code{method = 'pearson'}}{Correlation coefficient used for test.}
#'   \item{\code{type = c('rank', 'threshold')}}{Screen covariates by (1) rank
#'   (default), which chooses the top \code{num_screen} correlated covariates;
#'   or (2) threshold, which chooses covariates with a correlation- test- based
#'   pvalue lower the threshold and a minimum of \code{min_screen} covariates.}
#'   \item{\code{num_screen = 5}}{Number of covariates to select.}
#'   \item{\code{pvalue_threshold = 0.1}}{Maximum p-value threshold. Covariates
#'   with a pvalue lower than this threshold will be retained, and at least
#'   \code{min_screen} most significant covariates will be selected.}
#'   \item{\code{min_screen = 2}}{Minimum number of covariates to select. Used
#'   in pvalue_threshold screening procedure.}
#' }
#
Lrnr_screener_correlation <- R6Class(
  classname = "Lrnr_screener_correlation",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(method = "pearson",
                          type = c("rank", "threshold"),
                          num_screen = 5,
                          pvalue_threshold = 0.1, min_screen = 2) {
      check_correlation_screener_args(
        type, num_screen, pvalue_threshold, min_screen
      )
      params <- args_to_list()
      super$initialize(params = params)
    }
  ),

  private = list(
    .properties = c("binomial", "continuous", "categorical"),

    .train = function(task) {
      outcome_type <- self$get_outcome_type(task)
      X <- task$X
      Y <- outcome_type$format(task$Y)
      covs <- task$nodes$covariates

      args <- self$params
      type <- set_correlation_screener_type(args$type, args$num_screen)
      method <- args$method

      list_pvalues <- apply(X, 2, function(x, Y, method) {
        ifelse(var(x) <= 0, 1, cor.test(x, y = Y, method = method)$p.value)
      }, Y = Y, method = method)

      if (type == "rank") {
        selected <- (rank(list_pvalues) <= args$num_screen)
      } else if (type == "threshold") {
        selected <- (list_pvalues <= args$pvalue_threshold)
        if (sum(selected) < args$min_screen) {
          selected[rank(list_pvalues) <= args$min_screen] <- TRUE
        }
      }

      selected_names <- names(X)[selected]
      selected_covs <- sapply(covs, function(cov) any(grep(cov, selected_names)))
      fit_object <- list(selected = covs[selected_covs])
      return(fit_object)
    },

    .predict = function(task) {
      task$X[, private$.fit_object$selected, with = FALSE, drop = FALSE]
    },

    .chain = function(task) {
      return(task$next_in_chain(covariates = private$.fit_object$selected))
    }
  )
)

check_correlation_screener_args <- function(type, num, thresh, min) {

  # if type is specified
  if (length(type) == 1) {
    if (type == "rank" && is.null(num)) {
      stop("For type = rank, num_screen argument must be provided.")
    } else if (type == "threshold" && (is.null(thresh) | is.null(min))) {
      stop(
        "For type = threshold, pvalue_threshold and min_screen ",
        "arguments must be provided."
      )
    }
  }

  # if type is NULL or not specified
  if (length(type) > 1 || is.null(type)) {
    if (!is.null(num)) {
      message("Using default correlation screening type, rank.")
    } else {
      if (!is.null(thresh) && !is.null(min)) {
        message("Using correlation screening type, threshold.")
      } else {
        stop(
          "Missing arguments. For type = rank, provide num_screen.",
          " For  type = threshold, provide pvalue_threshold and min_screen."
        )
      }
    }
  }
}

set_correlation_screener_type <- function(type, num) {
  if (length(type) == 1) {
    new_type <- type
  }
  if (length(type) > 1 || is.null(type)) {
    new_type <- ifelse(!is.null(num), "rank", "threshold")
  }
  return(new_type)
}
