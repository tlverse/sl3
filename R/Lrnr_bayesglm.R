#' Bayesian Generalized Linear Models
#'
#' This learner provides fitting procedures for bayesian generalized linear
#' models (GLMs) from \pkg{ar} using \code{\link[arm]{bayesglm.fit}}. The GLMs
#' fitted in this way can incorporate independent normal, t, or Cauchy prior
#' distribution for the coefficients.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
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
#'  - \code{intercept = TRUE}: A \code{logical} specifying whether an intercept
#'      term should be included in the fitted null model.
#'  - \code{...}: Other parameters passed to \code{\link[arm]{bayesglm.fit}}.
#'      See it's documentation for details.
#'
#' @examples
#' data(cpp_imputed)
#' covars <- c(
#'   "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn"
#' )
#' outcome <- "haz"
#' task <- sl3_Task$new(cpp_imputed,
#'   covariates = covars,
#'   outcome = outcome
#' )
#' # fit a bayesian GLM
#' bayesglm_lrnr <- make_learner(Lrnr_bayesglm)
#' bayesglm_fit <- bayesglm_lrnr$train(task)
#' bayesglm_preds <- bayesglm_fit$predict(task)
Lrnr_bayesglm <- R6Class(
  classname = "Lrnr_bayesglm", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(intercept = TRUE, ...) {
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
      link_fun <- args$family$linkfun

      # specify data
      if (args$intercept) {
        args$x <- as.matrix(task$X_intercept)
      } else {
        args$x <- as.matrix(task$X)
      }
      args$y <- outcome_type$format(task$Y)

      if (task$has_node("weights")) {
        args$weights <- task$weights
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset_transformed(link_fun)
      }

      args$control <- glm.control(trace = FALSE)
      SuppressGivenWarnings(
        {
          fit_object <- call_with_args(arm::bayesglm.fit, args)
        },
        GetWarningsToSuppress()
      )

      fit_object$linkinv_fun <- linkinv_fun
      fit_object$link_fun <- link_fun
      fit_object$training_offset <- task$has_node("offset")

      return(fit_object)
    },

    .predict = function(task) {
      verbose <- getOption("sl3.verbose")
      if (self$params$intercept) {
        X <- task$X_intercept
      } else {
        X <- task$X
      }

      predictions <- rep.int(NA, nrow(X))
      if (nrow(X) > 0) {
        coef <- self$fit_object$coef
        if (!all(is.na(coef))) {
          eta <- as.matrix(X
          [, which(!is.na(coef)),
            drop = FALSE,
            with = FALSE
          ]) %*% coef[!is.na(coef)]

          if (self$fit_object$training_offset) {
            offset <- task$offset_transformed(self$fit_object$link_fun,
                                              for_prediction = TRUE)
            eta <- eta + offset
          }

          predictions <- as.vector(self$fit_object$linkinv_fun(eta))
        }
      }
      return(predictions)
    },

    .required_packages = c("arm")
  )
)
