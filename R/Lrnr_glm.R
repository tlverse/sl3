#' Generalized Linear Models
#'
#' This learner provides fitting procedures for generalized linear models using
#' the \pkg{stats} package \code{\link[stats]{glm.fit}} function.
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
#'   - \code{intercept = TRUE}: Should an intercept be included in the model?
#'     This argument is ignored when \code{formula} argument is not \code{NULL}.
#'   - \code{formula = NULL}: An object of class \code{formula} containing a
#'     description for the \code{\link[stats]{glm}} model to fit. NOTE that the
#'     \code{formula}'s response variable (i.e., variable before "\code{~}")
#'     must be identical to the outcome name as it's provided in the
#'     \code{task} and the \code{formula}'s regressors (i.e., variables after
#'     "\code{~}") must correspond to the column names in \code{X}, where
#'     \code{X} is the \code{task}'s processed dataset that's used for training
#'     learners and it can be accessed via \code{task$X}.
#'   - \code{...}: Other parameters passed to \code{\link[stats]{glm}} or
#'       \code{\link[stats]{glm.fit}}.
#'
#' @examples
#' data(cpp_imputed)
#' covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
#' task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")
#'
#' # simple, main-terms GLM
#' lrnr_glm <- make_learner(Lrnr_glm)
#' glm_fit <- lrnr_glm$train(task)
#' glm_preds <- glm_fit$predict()
#'
#' # We can include interaction terms by 'piping' them into this learner.
#' # Note that both main terms and the specified interactions will be included
#' # in the regression model.
#' interaction <- list(c("apgar1", "parity"))
#' lrnr_interaction <- Lrnr_define_interactions$new(interactions = interaction)
#' lrnr_glm_w_interaction <- make_learner(Pipeline, lrnr_interaction, lrnr_glm)
#' fit <- lrnr_glm_w_interaction$train(task)
#' coefs <- coef(fit$learner_fits$Lrnr_glm_TRUE)
Lrnr_glm <- R6Class(
  classname = "Lrnr_glm",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(intercept = TRUE, formula = NULL, ...) {
      super$initialize(params = args_to_list(), ...)
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
      if (!is.null(args$formula)) {
        if (class(args$formula) != "formula") {
          print("Converting the provided formula to formula object")
          args$formula <- as.formula(args$formula)
        }
        if (!all.vars(args$formula)[1] == task$nodes$outcome) {
          stop(paste0(
            "Formula outcome name ", all.vars(args$formula)[1],
            " does not match the task's outcome name ", task$nodes$outcome
          ))
        }
        if (!all(all.vars(args$formula)[-1] %in% colnames(task$X))) {
          stop("Regressor variables in the formula are not columns in task$X")
        }
        args$data <- data.frame(outcome_type$format(task$Y), task$X)
        colnames(args$data)[1] <- task$nodes$outcome
      } else {
        if (args$intercept) {
          args$x <- as.matrix(task$X_intercept)
        } else {
          args$x <- as.matrix(task$X)
        }
        args$y <- outcome_type$format(task$Y)
      }


      if (task$has_node("weights")) {
        args$weights <- task$weights
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset_transformed(link_fun)
      }

      args$control <- glm.control(trace = FALSE)

      if (!is.null(args$formula)) {
        SuppressGivenWarnings(
          {
            fit_object <- call_with_args(stats::glm, args)
          },
          GetWarningsToSuppress()
        )
      } else {
        SuppressGivenWarnings(
          {
            fit_object <- call_with_args(stats::glm.fit, args)
          },
          GetWarningsToSuppress()
        )
      }

      fit_object$linear.predictors <- NULL
      fit_object$weights <- NULL
      fit_object$prior.weights <- NULL
      fit_object$y <- NULL
      fit_object$residuals <- NULL
      fit_object$fitted.values <- NULL
      fit_object$effects <- NULL
      fit_object$qr$qr <- NULL
      fit_object$linkinv_fun <- linkinv_fun
      fit_object$link_fun <- link_fun
      fit_object$training_offset <- task$has_node("offset")
      return(fit_object)
    },
    .predict = function(task) {
      verbose <- getOption("sl3.verbose")

      if (self$params$intercept & is.null(self$params$formula)) {
        X <- task$X_intercept
      } else {
        X <- task$X
      }

      coef <- self$fit_object$coef

      if (nrow(X) > 0 & !all(is.na(coef))) {
        if (!is.null(self$params$formula)) {
          predictions <- stats::predict(
            private$.fit_object,
            newdata = X, type = "response"
          )
          if (task$outcome_type$type == "categorical") {
            predictions <- pack_predictions(predictions)
          }
        } else {
          X <- as.matrix(X[, which(!is.na(coef)), drop = FALSE, with = FALSE])
          eta <- X %*% coef[!is.na(coef)]
          if (self$fit_object$training_offset) {
            offset <- task$offset_transformed(
              self$fit_object$link_fun,
              for_prediction = TRUE
            )
            eta <- eta + offset
          }
          predictions <- as.vector(self$fit_object$linkinv_fun(eta))
        }
      } else {
        predictions <- rep.int(NA, nrow(X))
      }
      return(predictions)
    }
  )
)
