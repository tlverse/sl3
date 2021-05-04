#' Scalable Highly Adaptive Lasso (HAL)
#'
#' HAL is an estimation procedure that generates a design matrix consisting of
#' basis functions corresponding to covariates and interactions of covariates
#' and fits Lasso regression to this (usually) very wide matrix, recovering a
#' nonparametric functional form that describes the target prediction function
#' as a composition of subset functions with finite variation norm. This
#' implementation uses \pkg{hal9001}.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom origami folds2foldvec
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
#'   \item{\code{...}}{Arguments passed to \code{\link[hal9001]{fit_hal}}. See
#'   it's documentation for details.
#'   }
#' }
#
Lrnr_hal9001 <- R6Class(
  classname = "Lrnr_hal9001",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial", "weights", "ids"),

    .train = function(task) {
      args <- self$params

      args$X <- as.matrix(task$X)

      outcome_type <- self$get_outcome_type(task)
      args$Y <- outcome_type$format(task$Y)

      if (is.null(args$family)) {
        args$family <- outcome_type$glm_family()
      }

      if (!any(grepl("fit_control", names(args)))) {
        args$fit_control <- list()
      }
      args$fit_control$foldid <- origami::folds2foldvec(task$folds)

      if (task$has_node("id")) {
        args$id <- task$id
      }

      if (task$has_node("weights")) {
        args$fit_control$weights <- task$weights
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset
      }

      # fit HAL, allowing glmnet-fitting arguments
      other_valid <- c(
        names(formals(glmnet::cv.glmnet)), names(formals(glmnet::glmnet))
      )

      fit_object <- call_with_args(
        hal9001::fit_hal, args,
        other_valid = other_valid
      )

      return(fit_object)
    },

    .predict = function(task = NULL) {
      predictions <- predict(self$fit_object, new_data = as.matrix(task$X))
      if (!is.na(safe_dim(predictions)[2])) {
        p <- ncol(predictions)
        colnames(predictions) <- sprintf("lambda_%0.3e", self$params$lambda)
      }
      return(predictions)
    },

    .required_packages = c("hal9001", "glmnet")
  )
)
