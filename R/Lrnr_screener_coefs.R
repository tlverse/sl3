#' Coefficient Magnitude Screener
#'
#' This learner provides screening of covariates based on the magnitude of
#' their estimated coefficients in a (possibly regularized) GLM.
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
#'   \item{\code{learner}}{An instantiated learner to use for estimating
#'     coefficients used in screening.}
#'   \item{\code{threshold = 1e-3}}{Minimum size of coefficients to be kept.}
#'   \item{\code{max_screen = NULL}}{Maximum number of covariates to be kept.}
#'   \item{\code{min_screen = 2}}{Maximum number of covariates to be kept. Only
#'     applicable when supplied \code{learner} is a \code{\link{Lrnr_glmnet}}.}
#'   \item{\code{...}}{Other parameters passed to \code{learner}.}
#' }
Lrnr_screener_coefs <- R6Class(
  classname = "Lrnr_screener_coefs",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(learner, threshold = 0, max_screen = NULL,
                          min_screen = 2, ...) {
      super$initialize(params = args_to_list(), ...)
    }
  ),
  private = list(
    .properties = c("screener"),
    .train = function(task) {
      args <- self$params

      fit <- args$learner$train(task)
      coefs <- as.vector(coef(fit))
      coef_names <- rownames(coef(fit))
      if (is.null(coef_names)) {
        coef_names <- names(coef(fit))
      }

      if (is.null(coef_names)) {
        stop("could not extract names from fit coefficients")
      }

      covs <- task$nodes$covariates

      selected_coefs <- coef_names[which(abs(coefs) > args$threshold)]
      selected_coefs <- unique(gsub("\\..*", "", selected_coefs))
      selected <- intersect(selected_coefs, covs)

      if (!is.null(args$max_screen)) {
        if (args$max_screen < length(selected)) {
          ord_coefs <- coef_names[order(abs(coefs), decreasing = TRUE)]
          ord_coefs <- unique(gsub("\\..*", "", ord_coefs))
          selected <- intersect(ord_coefs, covs)[1:args$max_screen]
        }
      }

      if (length(selected) < args$min_screen) {
        if ("lambda" %in% names(args$learner$params)) {
          warning(
            "Less than min_screen covariates selected. Increasing ",
            "Lrnr_glmnet's lambda to select min_screen covariates."
          )
          selected <- list()
          for (i in 1:(length(task$X) - args$min_screen)) {
            selectedX <- get_selectedX(fit, args$threshold, i)
            selected[[i]] <- intersect(selectedX, covs)
            if (length(selected[[i]]) >= args$min_screen) {
              break
            }
          }
          selected <- selected[[length(selected)]]
          if (length(selected) < args$min_screen) {
            warning(
              "Could not increase Lrnr_glmnet's lambda enough select ",
              "min_screen covariates. Try increasing the values in ",
              "Lrnr_glmnet's lambda sequence, or decreasing min_screen. ",
              "Selecting all covariates."
            )
            selected <- covs
          }
        } else {
          warning(
            "Less than min_screen covariates selected, and supplied ",
            "learner is not Lrnr_glmnet, so lambda cannot be increased to ",
            "select min_screen covariates. Selecting all covariates."
          )
          selected <- covs
        }
      }

      fit_object <- list(selected = selected)
      return(fit_object)
    },
    .predict = function(task) {
      task$data[, private$.fit_object$selected, with = FALSE, drop = FALSE]
    },
    .chain = function(task) {
      return(task$next_in_chain(covariates = private$.fit_object$selected))
    },
    .required_packages = c()
  )
)

get_selectedX <- function(fit, threshold, min_screen) {
  coef_matrix <- fit$fit_object$glmnet.fit$beta
  new_lambda <- which.max(
    apply(coef_matrix, 2, function(x) sum(abs(x) > threshold)) >= min_screen
  )
  which_screen <- which(abs(coef_matrix[, new_lambda]) > threshold)
  selected_coefs <- rownames(coef_matrix)[which_screen]
  return(unique(gsub("\\..*", "", selected_coefs)))
}
