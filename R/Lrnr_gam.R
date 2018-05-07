#' GENERALIZED ADDITIVE MODELS
#'
#' This learner uses \code{\link[gam]{gam.fit}} from \code{gam} to fit my favorite machine learning algorithm.
#'
#' @docType class
#' @importFrom R6 R6Class gam family predict
#' @export
#' @keywords data
#' @return Learner object with methods for training and prediction. See \code{\link{Lrnr_base}} for documentation on learners.
#' @format \code{\link{R6Class}} object.
#' @family Learners
#'
#' @section Parameters:
#' \describe{
#'   \item{\code{...}}{Parameters passed to \code{\link[gam]{gam}}.}
#' }
#'
#'
#' @template common_parameters
Lrnr_gam <- R6Class(classname = "Lrnr_gam", inherit = Lrnr_base,
                    portable = TRUE, class = TRUE,
# Above, you should change Lrnr_template (in both the object name and the classname argument)
# to a name that indicates what your learner does
  public = list(

    # you can define default parameter values here
    # if possible, your learner should define defaults for all required parameters
    initialize = function(intercept = TRUE, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }

  ),
  private = list(
    # list properties your learner supports here.
    # Use sl3_list_properties() for a list of options
    .properties = c("continuous", "binomial", "weights", "offset"),

    # list any packages required for your learner here.
    .required_packages = c("gam"),

    # .train takes task data and returns a fit object that can be used to generate predictions
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

      # add task data to the argument list
      # what these arguments are called depends on the learner you are wrapping
      args$x <- as.matrix(task$X_intercept)
      args$y <- outcome_type$format(task$Y)

      # Sets family of distributon and appropriate linkage function
      # based on outcome_type if not specified by user
      if (is.null(args$family)) {
        args$family <- outcome_type$glm_family(return_object = TRUE)
      }
      family_name <- args$family$family
      linkinv_fun <- args$family$linkinv
      link_fun <- args$family$linkfun

      # only add arguments on weights and offset
      # if those were specified when the task was generated
      if(task$has_node("weights")){
        args$weights <- task$weights
      }

      if(task$has_node("offset")){
        args$offset <- task$offset
      }

      # call a function that fits your algorithm
      # with the argument list you constructed
      args$ctrl <- gam.control(trace = FALSE)
      SuppressGivenWarnings({
        fit_object <- call_with_args(gam::gam.fit, args)
      }, GetWarningsToSuppress())

      # return the fit object, which will be stored
      # in a learner object and returned from the call
      # to learner$predict
      # Lrnr_glm sets the following values to NULL,
      # So we do the same for Lrnr_gam
      fit_object$additive.predictors <- NULL
      fit_object$weights <- NULL
      fit_object$prior.weights <- NULL
      fit_object$y <- NULL
      fit_object$residuals <- NULL
      fit_object$fitted.values <- NULL
      fit_object$effects <- NULL
      fit_object$qr <- NULL
      fit_object$linkinv_fun <- linkinv_fun
      fit_object$link_fun <- link_fun
      fit_object$training_offset <- task$has_node("offset")
      return(fit_object)
    },

    # .predict takes a task and returns predictions from that task
    .predict = function(task = NULL) {
      self$training_task
      self$training_outcome_type
      self$fit_object

      predictions <- predict(self$fit_object, task$X)
      return(predictions)
    }
  )
)
