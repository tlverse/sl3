#' The Scalable Highly Adaptive LASSO
#'
#' The Highly Adaptive LASSO is an estimation procedure that generates a design
#'  matrix consisting of basis functions corresponding to covariates and
#'  interactions of covariates and fits LASSO regression to this (usually) very
#'  wide matrix, recovering a nonparametric functional form that describes the
#'  target prediction function as a composition of subset functions with finite
#'  variation norm. This implementation uses the \code{hal9001} R package, which
#'  provides both a custom implementation (based on the \code{origami} package)
#'  of the CV-LASSO as well the standard call to \code{cv.glmnet} from the
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
#'   \item{\code{degrees="degrees"}}{ The highest order of interaction terms for
#'    which the basis functions ought to be generated. The default (\code{3})
#'    corresponds to generating basis functions up to 3-way interactions between
#'    all covariates. Note that this differs from the default \code{NULL} used
#'    in \code{hal9001::fit_hal}.
#'   }
#'   \item{\code{fit_type="fit_type"}}{ The specific routine to be called when
#'    fitting the LASSO regression in a cross-validated manner. Choosing the
#'    \code{glmnet} option will result in a call to \code{cv.glmnet} while
#'    \code{origami} will produce a (faster) call to a custom routine based on
#'    the \code{origami} package.
#'   }
#'   \item{\code{n_folds="n_folds"}}{ Integer for the number of folds to be used
#'    when splitting the data for cross-validation. This defaults to 10 as this
#'    is the convention for v-fold cross-validation.
#'   }
#'   \item{\code{use_min="use_min"}}{ Determines which lambda is selected from
#'    \code{cv.glmnet}. \code{TRUE} corresponds to \code{"lambda.min"} and
#'    \code{FALSE} corresponds to \code{"lambda.1se"}.
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
    initialize = function(degrees = 3,
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
        args$family <- outcome_type$glm_family(return_object = TRUE)$family
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
