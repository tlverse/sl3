#' Generalized Random Forests for Conditional Average Treatment Effects
#'
#' This learner implements the so-called "Causal Forests" estimator of the
#' conditional average treatment effect (CATE) using the \pkg{grf} package
#' function \code{\link[grf]{causal_forest}}. This learner is intended for use
#' in the \code{tmle3mopttx} package, where it is necessary to fit the CATE,
#' and then predict CATE values from new covariate data. As such, this learner
#' requires a treatment/exposure node to be specified (\code{A}).
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stats predict
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @export
#'
#' @keywords data
#'
#' @return A learner object inheriting from \code{\link{Lrnr_base}} with
#'  methods for training and prediction. For a full list of learner
#'  functionality, see the complete documentation of \code{\link{Lrnr_base}}.
#'
#' @format An \code{\link[R6]{R6Class}} object inheriting from
#'  \code{\link{Lrnr_base}}.
#'
#' @family Learners
#'
#' @section Parameters:
#'  - \code{A}: Column name in the \code{sl3_Task}'s \code{covariates} that
#'     indicates the treatment/exposure of interest. The treatment assignment
#'     must be a binary or real numeric vector with no NAs.
#'  - \code{...}: Other parameters passed to \code{\link[grf]{causal_forest}}.
#'     See its documentation for details.
#'
#' @examples
#' data(mtcars)
#' mtcars_task <- sl3_Task$new(
#'   data = mtcars,
#'   covariates = c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am"),
#'   outcome = "mpg"
#' )
#' # simple prediction with lasso penalty
#' grfcate_lrnr <- Lrnr_grfcate$new(A = "vs")
#' grfcate_fit <- grfcate_lrnr$train(mtcars_task)
#' grf_cate_predictions <- grfcate_fit$predict()
Lrnr_grfcate <- R6Class(
  classname = "Lrnr_grfcate",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(A, ...) {
      super$initialize(params = args_to_list(), ...)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial", "categorical", "weights"),
    .train = function(task) {
      args <- self$params

      # specify outcome
      outcome_type <- self$get_outcome_type(task)
      args$Y <- outcome_type$format(task$Y)

      # check that A is in the task covariates
      if (!(args$A %in% task$nodes$covariates)) {
        stop(sprintf(
          "A, %s, must be specified as a covariate in sl3_Task", paste0(args$A)
        ))
      }
      # specify covariates excluding A
      args$X <- task$X[, -args$A, with = FALSE]
      # specify A
      args$W <- task$X[[args$A]]

      if (task$has_node("weights")) {
        args$weights <- task$weights
      }

      # train via call_with_args and return fitted object
      fit_object <- call_with_args(grf::causal_forest, args, ignore = "A")
      return(fit_object)
    },
    .predict = function(task) {

      # remove A from prediction data if it's present
      if (self$params$A %in% task$nodes$covariates) {
        newX <- task$X[, -self$params$A, with = FALSE]
      } else {
        newX <- task$X
      }

      # make sure newX contains the same covariates used for training
      trainX_names <- names(private$.fit_object$X.orig)
      if (!all(trainX_names %in% names(newX))) {
        missing_names <- trainX_names[!(trainX_names %in% names(newX))]
        stop(sprintf(
          "Prediction task covariates missing covariates in training task: %s",
          paste0(missing_names, collapse = ", ")
        ))
      }
      # remove any newX covariates that were not used for training
      if (!all(names(newX) %in% trainX_names)) {
        new_covs <- names(newX)[!(names(newX) %in% trainX_names)]
        warning(sprintf(
          "Prediction task has new covariates not seen in training: %s;\nDropping these for prediction",
          paste0(new_covs, collapse = ", ")
        ))
        newX <- newX[, -new_covs, with = F]
      }
      # set newX order of covariate to match order of training covariates
      newX[, order(trainX_names), with = FALSE]

      # generate predictions and output
      predictions_list <- stats::predict(
        private$.fit_object,
        new_data = newX
      )
      predictions <- as.numeric(predictions_list$predictions)
      return(predictions)
    },
    .required_packages = c("grf")
  )
)
