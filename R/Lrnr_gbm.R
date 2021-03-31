#' GBM: Generalized Boosted Regression Models
#'
#' This learner provides fitting procedures for generalized boosted regression
#' trees, using the routines from \pkg{gbm}, through a call to the function
#' \code{\link[gbm]{gbm.fit}}. Though a variety of gradient boosting strategies
#' have seen popularity in machine learning, a few of the early methodological
#' descriptions were given by \insertCite{friedman-gbm1;textual}{sl3} and
#' \insertCite{friedman-gbm2;textual}{sl3}.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stats predict
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
#' @seealso [Lrnr_xgboost] for extreme gradient boosting via \pkg{xgboost}
#'
#' @section Parameters:
#'   - \code{n.trees}: An integer specifying the total number of trees to fit.
#'       This is equivalent to the number of iterations and the number of basis
#'       functions in the additive expansion. The default is 10000.
#'   - \code{interaction.depth}: An integer specifying the maximum depth of
#'       each tree (i.e., the highest level of allowed variable interactions).
#'       A value of 1 implies an additive model, while a value of 2 implies a
#'       model with up to 2-way interactions, etc. The default is 2.
#'   - \code{shrinkage}: A shrinkage parameter applied to each tree in the
#'       expansion. Also known as the learning rate or step-size reduction;
#'       values of 0.001 to 0.1 have been found to usually work, but a smaller
#'       learning rate typically requires more trees. The default is 0.001.
#'   - \code{...}: Other parameters passed to \code{\link[gbm]{gbm}}. See its
#'       documentation for details.
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' data(cpp_imputed)
#' # create task for prediction
#' cpp_task <- sl3_Task$new(
#'   data = cpp_imputed,
#'   covariates = c("apgar1", "apgar5", "parity", "gagebrth", "mage", "sexn"),
#'   outcome = "haz"
#' )
#' # initialization, training, and prediction with the defaults
#' gbm_lrnr <- Lrnr_gbm$new()
#' gbm_fit <- gbm_lrnr$train(cpp_task)
#' gbm_preds <- gbm_fit$predict()
Lrnr_gbm <- R6Class(
  classname = "Lrnr_gbm", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(n.trees = 10000L, interaction.depth = 2,
                          shrinkage = 0.001, ...) {
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
        args$w <- task$weights
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset
      }

      if (is.null(args$distribution)) {
        if (outcome_type$type == "continuous") {
          args$distribution <- "gaussian"
        } else if (outcome_type$type == "binomial") {
          args$distribution <- "bernoulli"
        } else {
          stop("Unsupported outcome type for Lrnr_gbm.")
        }
      }

      if (is.null(args$verbose)) {
        args$verbose <- getOption("sl3.verbose")
      }

      fit_object <- call_with_args(gbm::gbm.fit, args)
      return(fit_object)
    },

    .predict = function(task) {
      preds <- stats::predict(
        object = private$.fit_object, newdata = task$X,
        n.trees = self$params$n.trees, type = "response"
      )
      return(preds)
    },
    .required_packages = c("gbm")
  )
)
