#' Computationally Efficient hal9001
#'
#' This learner uses \code{\link[hal9001]{fit_hal}} from \code{hal9001} to fit my favorite machine learning algorithm.
#' The procedure uses a custom C++ implementation to generate a design
#'  matrix (consisting of basis functions corresponding to covariates and
#'  interactions of covariates) and remove duplicate columns of indicators. The
#'  LASSO regression is fit to this (usually) very wide matrix using either a
#'  custom implementation (based on the \code{origami} package) or by a call to
#'  \code{cv.glmnet} from the \code{glmnet} package.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return Learner object with methods for training and prediction. See \code{\link{Lrnr_base}} for documentation on learners.
#' @format \code{\link{R6Class}} object.
#' @family Learners
#'
#' @section Parameters:
#' \describe{
#'   \item{\code{degrees="degrees"}}{ The highest order of interaction terms for which the basis functions ought to be generated. The default (\code{NULL}) corresponds to generating basis functions for the full dimensionality of the input matrix.
#'   }
#'   \item{\code{fit_type="fit_type"}}{ The specific routine to be called when fitting the LASSO regression in a cross-validated manner. Choosing the \code{glmnet} option will result in a call to \code{cv.glmnet} while \code{origami} will produce a (faster) call to a custom routine based on the \code{origami} package.
#'   }
#'   \item{\code{n_folds="n_folds"}}{ Integer for the number of folds to be used when splitting the data for cross-validation. This defaults to 10 as this is the convention for v-fold cross-validation.
#'   }
#'   \item{\code{use_min="use_min"}}{ Determines which lambda is selected from \code{cv.glmnet}. \code{TRUE} corresponds to \code{"lambda.min"} and \code{FALSE} corresponds to \code{"lambda.1se"}.
#'   }
#'   \item{\code{...}}{ Other parameters passed directly to \code{\link[hal9001]{fit_hal}}. See its documentation for details.
#'   }
#' }
#'
Lrnr_hal9001 <- R6Class(classname = "Lrnr_hal9001", inherit = Lrnr_base,
                    portable = TRUE, class = TRUE,
# Above, you should change Lrnr_hal9001 (in both the object name and the classname argument)
# to a name that indicates what your learner does
  public = list(
    initialize = function(degrees = NULL,
                          fit_type = c("origami", "glmnet"),
                          n_folds = 10,
                          use_min = TRUE,
                          ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }

    # you can define public functions that allow your learner to do special things here
    # for instance glm learner might return prediction standard errors
    # special_function = function(arg_1){
    # }
  ),
  private = list(
    # list properties your learner supports here.
    # Use sl3_list_properties() for a list of options
    # .properties = c("continuous", "binomial"),
    .properties = c("continuous"),

    .required_packages = c("hal9001"),

    .train = function(task) {
      # generate an argument list from the parameters that were
      # captured when your learner was initialized.
      # this allows users to pass arguments directly to your ml function
      args <- self$params

      # get outcome variable type
      # prefering learner$params$outcome_type first, then task$outcome_type
      outcome_type <- self$get_outcome_type(task)
      # should pass something on to your learner indicating outcome_type
      # e.g. family or objective
      if (is.null(args$family)) {
        args$family <- outcome_type$glm_family(return_object = TRUE)
      }
      family_name <- args$family$family
      linkinv_fun <- args$family$linkinv
      link_fun <- args$family$linkfun

      # add task data to the argument list
      # what these arguments are called depends on the learner you are wrapping
      args$X <- as.matrix(task$X_intercept)
      args$Y <- outcome_type$format(task$Y)
      args$yolo <- FALSE

      # only add arguments on weights and offset
      # if those were specified when the task was generated
      if(task$has_node("weights")){
        args$obsWeights <- task$weights
      }

      if(task$has_node("offset")){
        args$offset <- task$offset
      }

      # call a function that fits your algorithm
      # with the argument list you constructed
      SuppressGivenWarnings({
        fit_object <- try(
          call_with_args(hal9001::fit_hal, args),
          silent = TRUE
        )
      }, GetWarningsToSuppress())

      # return the fit object, which will be stored
      # in a learner object and returned from the call
      # to learner$predict
      return(fit_object)
    },

    # .predict takes a task and returns predictions from that task
    .predict = function(task = NULL) {
      # self$training_task
      # self$training_outcome_type
      # self$fit_object
      X <- task$X_intercept
      predictions <- rep.int(NA, nrow(X))
      if (nrow(X) > 0) predictions <- stats::predict(self$fit_object, new_data = X)

      return(predictions)
    }
  )
)

