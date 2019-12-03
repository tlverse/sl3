#' The Scalable Highly Adaptive Lasso
#'
#' The Highly Adaptive Lasso is an estimation procedure that generates a design
#'  matrix consisting of basis functions corresponding to covariates and
#'  interactions of covariates and fits Lasso regression to this (usually) very
#'  wide matrix, recovering a nonparametric functional form that describes the
#'  target prediction function as a composition of subset functions with finite
#'  variation norm. This implementation uses the \code{hal9001} R package, which
#'  provides both a custom implementation (based on the \code{origami} package)
#'  of the CV-Lasso as well the standard call to \code{cv.glmnet} from the
#'  \code{glmnet} package.
#'
#' @docType class
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
#'   \item{\code{max_degree=3}}{ The highest order of interaction
#'    terms for which the basis functions ought to be generated. The default
#'    corresponds to generating basis functions up to all 3-way interactions of
#'    covariates in the input matrix, matching the default in \pkg{hal9001}.
#'   }
#'   \item{\code{fit_type="glmnet"}}{The specific routine to be called when
#'    fitting the Lasso regression in a cross-validated manner. Choosing the
#'    "glmnet" option calls either \code{\link[glmnet]{cv.glmnet}} or
#'    \code{\link[glmnet]{glmnet}}.
#'   }
#'   \item{\code{n_folds=10}}{Integer for the number of folds to be used
#'    when splitting the data for cross-validation. This defaults to 10 as this
#'    is the convention for V-fold cross-validation.
#'   }
#'   \item{\code{use_min=TRUE}}{Determines which lambda is selected from
#'    \code{\link[glmnet]{cv.glmnet}}. \code{TRUE} corresponds to
#'    \code{"lambda.min"} and \code{FALSE} corresponds to \code{"lambda.1se"}.
#'   }
#'   \item{\code{basis_list=NULL}}{
#'   }
#'   \item{\code{...}}{ Other parameters passed directly to
#'    \code{\link[hal9001]{fit_hal}}. See its documentation for details.
#'   }
#' }
#
Lrnr_hal9001 <- R6Class(
  classname = "Lrnr_hal9001", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(max_degree = 3,
                          fit_type = "glmnet",
                          n_folds = 10,
                          use_min = TRUE,
                          basis_list = NULL,
                          ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial"),

    .train = function(task) {
      args <- self$params

      outcome_type <- self$get_outcome_type(task)

      if (is.null(args$family)) {
        args$family <- args$family <- outcome_type$glm_family()
      }

      args$X <- as.matrix(task$X)
      args$Y <- outcome_type$format(task$Y)
      args$yolo <- FALSE

      if (task$has_node("weights")) {
        args$weights <- task$weights
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset
      }

      fit_object <- call_with_args(hal9001::fit_hal, args)
      return(fit_object)
    },
    .predict = function(task = NULL) {
      predictions <- predict(self$fit_object, new_data = as.matrix(task$X))
      return(predictions)
    },
    .required_packages = c("hal9001")
  )
)
