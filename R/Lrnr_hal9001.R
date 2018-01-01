#' Template of a \code{sl3} Learner.
#'
#' This is a template for defining a new learner.
#' This can be copied to a new file using \code{\link{write_learner_template}}.
#' The remainder of this documentation is an example of how you might write documentation for your new learner.
#' This learner uses \code{\link[my_package]{my_ml_fun}} from \code{my_package} to fit my favorite machine learning algorithm.
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
#'   \item{\code{param_1="default_1"}}{ This parameter does something.
#'   }
#'   \item{\code{param_2="default_2"}}{ This parameter does something else.
#'   }
#'   \item{\code{...}}{ Other parameters passed directly to \code{\link[my_package]{my_ml_fun}}. See its documentation for details.
#'   }
#' }
#'
#' @section Methods:
#' \describe{
#' \item{\code{special_function(arg_1)}}{
#'   My learner is special so it has a special function.
#'
#'   \itemize{
#'     \item{\code{arg_1}: A very special argument.
#'    }
#'   }
#'   }
#' }
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

      predictions <- stats::predict(self$fit_object, task$X)
      return(predictions)
    }
  )
)

