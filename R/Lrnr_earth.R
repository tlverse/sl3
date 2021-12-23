#' Earth: Multivariate Adaptive Regression Splines
#'
#' This learner provides fitting procedures for building regression models thru
#' the spline regression techniques described in
#' \insertCite{friedman1991multivariate;textual}{sl3} and
#' \insertCite{friedman1993fast;textual}{sl3}, via \pkg{earth} and the function
#' \code{\link[earth]{earth}}.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stats predict gaussian binomial
#' @importFrom utils getS3method
#'
#' @export
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
#' @references
#'  \insertAllCited{}
#'
#' @section Parameters:
#'   - \code{degree}: A \code{numeric} specifying the maximum degree of
#'       interactions to be used in the model. This defaults to 2, specifying
#'       up through one-way interaction terms. Note that this differs from the
#'       default of \code{\link[earth]{earth}}.
#'   - \code{penalty}: Generalized Cross Validation (GCV) penalty per knot.
#'       Defaults to 3 as per the recommendation for \code{degree} > 1 in the
#'       documentation of \code{\link[earth]{earth}}. Special values (for use
#'       by knowledgeable users): The value 0 penalizes only terms, not knots.
#'       The value -1 translates to no penalty.
#'   - \code{pmethod}: Pruning method, defaulting to \code{"backward"}. Other
#'       options include \code{"none"}, \code{"exhaustive"}, \code{"forward"},
#'       \code{"seqrep"}, \code{"cv"}.
#'   - \code{nfold}: Number of cross-validation folds. The default is 0, for no
#'       cross-validation.
#'   - \code{ncross}: Only applies if \code{nfold} > 1, indicating the number
#'       of cross-validation rounds. Each cross-validation has \code{nfold}
#'       folds. Defaults to 1.
#'   - \code{minspan}: Minimum number of observations between knots.
#'   - \code{endspan}: Minimum number of observations before the first and
#'       after the final knot.
#'   - \code{...}: Other parameters passed to \code{\link[earth]{earth}}. See
#'       its documentation for details.
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
#' # fit and predict from a MARS model
#' earth_lrnr <- make_learner(Lrnr_earth)
#' earth_fit <- earth_lrnr$train(task)
#' earth_preds <- earth_fit$predict(task)
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
      earth_fun <- utils::getS3method("earth", "default",
        envir = getNamespace("earth")
      )

      # incorporate arguments defined by non-default earth function class
      default_args <- names(formals(earth_fun))
      extra_args <- names(formals(
        utils::getS3method("earth", "fit", envir = getNamespace("earth"))
      ))
      extra_args <- extra_args[!(extra_args %in% default_args)]

      fit_object <- call_with_args(earth_fun, args, other_valid = extra_args)
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
