#' Computationally Efficient Generalized Linear Model (GLM) Fitting
#'
#' This learner provides faster procedures for fitting linear and generalized
#' linear models than \code{\link{Lrnr_glm}} with a minimal memory footprint.
#' This learner uses the internal fitting function provided by \pkg{speedglm}
#' package, \code{\link[speedglm]{speedglm.wfit}}. See
#' \insertCite{speedglm;textual}{sl3} for more detail. The
#' \code{\link[stats]{glm.fit}} function is used as a fallback, if
#' \code{\link[speedglm]{speedglm.wfit}} fails.
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
#'   - \code{method = "Cholesky"}: The method to check for singularity.
#'   - \code{...}: Other parameters to be passed to
#'       \code{\link[speedglm]{speedglm.wfit}}.
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' data(cpp_imputed)
#' covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
#' task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")
#'
#' # simple, main-terms GLM
#' lrnr_glm_fast <- Lrnr_glm_fast$new(method = "eigen")
#' glm_fast_fit <- lrnr_glm_fast$train(task)
#' glm_fast_preds <- glm_fast_fit$predict()
Lrnr_glm_fast <- R6Class(
  classname = "Lrnr_glm_fast",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(intercept = TRUE, method = "Cholesky", ...) {
      super$initialize(params = args_to_list(), ...)
    }
  ),
  private = list(
    .default_params = list(method = "Cholesky"),
    .properties = c("continuous", "binomial", "weights", "offset"),
    .train = function(task) {
      verbose <- getOption("sl3.verbose")

      args <- self$params

      outcome_type <- self$get_outcome_type(task)
      args$y <- outcome_type$format(task$Y)

      if (is.null(args$family)) {
        args$family <- outcome_type$glm_family(return_object = TRUE)
      }
      family_name <- args$family$family
      linkinv_fun <- args$family$linkinv
      link_fun <- args$family$linkfun

      # specify data
      if (args$intercept) {
        args$X <- as.matrix(task$X_intercept)
      } else {
        args$X <- as.matrix(task$X)
      }

      if (task$has_node("weights")) {
        args$weights <- task$weights
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset_transformed(link_fun)
      }

      SuppressGivenWarnings(
        {
          fit_object <- try(
            call_with_args(speedglm::speedglm.wfit, args),
            silent = TRUE
          )
        },
        GetWarningsToSuppress()
      )

      if (inherits(fit_object, "try-error")) {
        # if failed, fall back on stats::glm
        if (verbose) {
          message(paste(
            "speedglm::speedglm.wfit failed, falling back on",
            "stats::glm.fit;", fit_object
          ))
        }
        args$ctrl <- glm.control(trace = FALSE)
        args$x <- args$X

        SuppressGivenWarnings(
          {
            fit_object <- call_with_args(stats::glm.fit, args)
          },
          GetWarningsToSuppress()
        )

        fit_object$linear.predictors <- NULL
        fit_object$weights <- NULL
        fit_object$prior.weights <- NULL
        fit_object$y <- NULL
        fit_object$residuals <- NULL
        fit_object$fitted.values <- NULL
        fit_object$effects <- NULL
        fit_object$qr <- NULL
      }

      fit_object$linkinv_fun <- linkinv_fun
      fit_object$link_fun <- link_fun
      fit_object$training_offset <- task$has_node("offset")
      return(fit_object)
    },
    .predict = function(task = NULL) {
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
            offset <- task$offset_transformed(self$fit_object$link_fun, for_prediction = TRUE)
            eta <- eta + offset
          }

          predictions <- as.vector(self$fit_object$linkinv_fun(eta))
        }
      }
      return(predictions)
    },
    .required_packages = c("speedglm")
  )
)
