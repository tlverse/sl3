#' Generalized Additive Models
#'
#' This learner uses \code{\link[gam]{gam.fit}} from the \code{gam} package to
#' efficiently fit generalized additive models.
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
#'   \item{\code{start}}{Starting values for the parameters in the additive
#'     predictor.}
#'   \item{\code{etastart}}{Starting values for the additive predictor.}
#'   \item{\code{mustart}}{Starting values for the vector of means.}
#'   \item{\code{...}}{Extra parameters passed to \code{\link[gam]{gam.fit}}.}
#' }
#
Lrnr_gam <- R6Class(classname = "Lrnr_gam",
                    inherit = Lrnr_base,
                    portable = TRUE,
                    class = TRUE,
  public = list(
    initialize = function(start = NULL,
                          etastart = NULL,
                          mustart = NULL,
                          ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial", "weights", "offset"),

    .train = function(task) {
      # generate an argument list from the parameters that were
      # captured when your learner was initialized.
      # this allows users to pass arguments directly to your ml function
      args <- self$params

      # get outcome variable type
      # preferring learner$params$outcome_type first, then task$outcome_type
      outcome_type <- self$get_outcome_type(task)

      # add task data to the argument list
      # what these arguments are called depends on the learner you are wrapping
      args$formula <- as.formula("task$Y ~ as.matrix(task$X)")

      # Sets family of distributon and appropriate linkage function
      # based on outcome_type if not specified by user
      if (is.null(args$family)) {
        args$family <- outcome_type$glm_family(return_object = TRUE)
      }

      if (task$has_node("weights")) {
        args$weights <- task$weights
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset
      }

      args$ctrl <- gam::gam.control(trace = FALSE)

      # perform model fitting with gam::gam.fit
      SuppressGivenWarnings({
        fit_object <- call_with_args(gam::gam, args)
      }, GetWarningsToSuppress())

      # return the fit object, which will be stored in a learner object and
      # returned from the call to learner$predict
      # NOTE: Lrnr_glm sets the following to NULL -- doing the same for Lrnr_gam
      fit_object$additive.predictors <- NULL
      fit_object$weights <- NULL
      fit_object$prior.weights <- NULL
      fit_object$y <- NULL
      fit_object$residuals <- NULL
      fit_object$fitted.values <- NULL
      fit_object$effects <- NULL
      fit_object$training_offset <- task$has_node("offset")
      return(fit_object)
    },

    # .predict takes a task and returns predictions from that task
    .predict = function(task = NULL) {
      self$training_task
      self$training_outcome_type
      self$fit_object

      predictions <- predict(self$fit_object, task$X)
      return(as.numeric(predictions))
    },
    .required_packages = c("gam")
  )
)
