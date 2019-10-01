#' Earth - multivariate adaptive regression splines
#'
#' This learner provides fitting procedures for building regression models using
#' the techniques in Friedman’s papers "Multivariate Adaptive Regres-sion
#' Splines" and "Fast MARS", using the function \code{\link[earth]{earth}} from
#' the \code{earth} package.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stats predict gaussian binomial
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
#'   \item{\code{degree}}{A \code{numeric} specifying the maximum degree of
#'     interactions to be used in the model. This defaults to 2, specifying up
#'     through one-way interaction terms. Note that this differs from the
#'     default of \code{earth::earth}.
#'   }
#'   \item{\code{penalty}}{Generalized Cross Validation (GCV) penalty per knot.
#'     Defaults to 3 as recommended for \code{degree} > 1 in the documentation
#'     of \code{earth::earth}. Special values (for use by knowledgeable users):
#'     The value 0 penalizes only terms, not knots. The value -1 means no
#'     penalty.
#'   }
#'   \item{\code{pmethod}}{Pruning method, defaulting to \code{"backward"}.
#'     Other options include \code{"none"}, \code{"exhaustive"},
#      \code{"forward"}, \code{"seqrep"}, \code{"cv"}.
#'   }
#'   \item{\code{nfold}}{Number of cross-validation folds. Default is0, no
#'     cross validation.
#'   }
#'   \item{\code{ncross}}{Only applies if \code{nfold} > 1. Number of
#'     cross-validations. Each cross-validation has \code{nfold} folds.
#'     Defaults to 1.
#'   }
#'   \item{\code{minspan}}{Minimum number of observations between knots.
#'   }
#'   \item{\code{endspan}}{Minimum number of observations before the first and
#'     after the final knot.
#'   }
#'   \item{\code{...}}{Other parameters passed to \code{\link[earth]{earth}}.
#'     See its documentation for details.
#'   }
#' }
#
Lrnr_earth <- R6Class(
  classname = "Lrnr_earth", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(degree = 2, penalty = 3, pmethod = "backward",
                              nfold = 0, ncross = 1, minspan = 0, endspan = 0,
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
      args$x <- task$X
      args$y <- outcome_type$format(task$Y)

      if (task$has_node("weights")) {
        args$weights <- task$weights
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset
      }

      if (outcome_type$type == "continuous") {
        glm <- list(family = stats::gaussian)
      } else if (outcome_type$type == "binomial") {
        glm <- list(family = stats::binomial)
      } else {
        stop("Unsupported outcome type for Lrnr_earth.")
      }
      args$glm <- glm

      fit_object <- do.call(getS3method("earth", "default"), args)
      return(fit_object)
    },

    .predict = function(task) {
      preds <- stats::predict(
        object = private$.fit_object, newdata = task$X,
        type = "response"
      )
      return(preds)
    },
    .required_packages = c("earth")
  )
)
