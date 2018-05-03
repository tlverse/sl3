#' Learner for Recursive Partitioning and Regression Trees.
#'
#' This learner uses \code{\link[rpart]{rpart}} from \code{rpart} to fit
#' recursive partioning and regression trees.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom stats predict as.formula
#' @export
#' @keywords data
#' @return Learner object with methods for training and prediction. See
#'  \code{\link{Lrnr_base}} for documentation on learners.
#' @format \code{\link{R6Class}} object.
#' @family Learners
#'
#' @section Parameters:
#' \describe{
#'   \item{\code{model}}{If logical: keep a copy of the model frame in the
#'     result?
#'   }
#'   \item{\code{x}}{Whether to keep a copy of the x matrix in the result.
#'   }
#'   \item{\code{y}}{Whether to keep a copy of the dependent variable in the
#'     result.
#'   }
#'   \item{\code{...}}{ Other parameters to be passed directly to
#'     \code{\link[rpart]{rpart}}. See its documentation for details.
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
Lrnr_rpart <- R6Class(
  classname = "Lrnr_rpart", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  # Above, you should change Lrnr_template (in both the object name and the classname argument)
  # to a name that indicates what your learner does
  public = list(
    # you can define default parameter values here
    # if possible, your learner should define defaults for all required parameters
    initialize = function(model = FALSE,
                              x = FALSE,
                              y = FALSE,
                              ...) {
      # this captures all parameters to initialize and saves them as self$params
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    # list properties your learner supports here.
    # Use sl3_list_properties() for a list of options
    .properties = c("continuous", "categorical", "binomial", "weights"),

    # .train takes task data and returns a fit object that can be used to generate predictions
    .train = function(task) {
      # generate an argument list from the parameters that were
      # captured when your learner was initialized.
      # this allows users to pass arguments directly to your ml function
      args <- self$params

      # get outcome variable type
      # preferring learner$params$outcome_type first, then task$outcome_type
      outcome_type <- self$get_outcome_type(task)
      # should pass something on to your learner indicating outcome_type
      # e.g. family or objective

      # add task data to the argument list
      # what these arguments are called depends on the learner you are wrapping
      x <- as.matrix(task$X)
      y <- outcome_type$format(task$Y)
      args$formula <- stats::as.formula(y ~ x)

      # only add arguments on weights and offset
      # if those were specified when the task was generated
      if (outcome_type$type == "binary") {
        args$y <- factor(y, levels = c(0, 1))
      }

      if (task$has_node("weights")) {
        args$weights <- task$weights
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset
      }

      # call a function that fits your algorithm
      # with the argument list you constructed
      fit_object <- call_with_args(rpart::rpart, args)

      # return the fit object, which will be stored
      # in a learner object and returned from the call
      # to learner$predict
      return(fit_object)
    },

    # .predict takes a task and returns predictions from that task
    .predict = function(task = NULL) {
      self$training_task
      self$training_outcome_type
      self$fit_object
      predictions <- stats::predict(self$fit_object, task$X)
      return(predictions)
    },
    .required_packages = c("rpart")
  )
)
