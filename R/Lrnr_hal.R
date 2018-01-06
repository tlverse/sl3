#' Generalized Linear Models
#'
#' This learner provides fitting procedures for the highly adaptive lasso
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stats glm predict family
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
#'   \item{\code{...}}{Parameters passed to \code{\link[stats]{glm}}.}
#' }
#'
#' @template common_parameters
#
Lrnr_hal <- R6Class(
  classname = "Lrnr_hal", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),

  private = list(
    .properties = c("continuous", "binomial", "weights", "offset"),

    .train = function(task) {
      args <- self$params
      outcome_type <- self$get_outcome_type(task)

      if (is.null(args$family)) {
        args$family <- outcome_type$glm_family(return_object = TRUE)
      }
      family_name <- args$family$family
      linkinv_fun <- args$family$linkinv
      # specify data
      args$X <- as.matrix(task$X)
      args$Y <- outcome_type$format(task$Y)
      args$newX = NULL
      args$verbose = FALSE
      args$obsWeights = rep(1, length(args$Y))
      args$nfolds = ifelse(length(args$Y) <= 100, 20, 10)
      args$nlambda = 100
      args$useMin = TRUE
      args$debug = TRUE
      args$parallel = FALSE

      if (task$has_node("weights")) {
        args$weights <- task$weights
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset
      }

      args$ctrl <- glm.control(trace = FALSE)
      SuppressGivenWarnings({
        fit_object <- call_with_args(hal::halplus, args)
      }, GetWarningsToSuppress())

      fit_object$linear.predictors <- NULL
      fit_object$weights <- NULL
      fit_object$prior.weights <- NULL
      fit_object$y <- NULL
      fit_object$residuals <- NULL
      fit_object$fitted.values <- NULL
      fit_object$effects <- NULL
      fit_object$qr <- NULL
      # fit_object$linkinv_fun <- linkinv_fun

      return(fit_object)
    },

    .predict = function(task = NULL) {
      verbose <- getOption("sl3.verbose")
      newX <- as.matrix(task$X)
      predictions <- rep.int(NA, nrow(newX))
      if (nrow(newX) > 0) {
        if (task$has_node("offset")) {
          predictions <- predict(private$.fit_object,
                                 newdata = newX,
                                 bigDesign = FALSE,
                                 chunks = 10000,
                                 offset = task$offset)
        } else {
          predictions <- predict(private$.fit_object,
                                 newdata = newX,
                                 bigDesign = FALSE,
                                 chunks = 10000
                                 )
        }
      }
      return(predictions)
    },
    .required_packages = c("hal")
  )
)
