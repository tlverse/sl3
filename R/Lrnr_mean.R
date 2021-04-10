#' Fitting Intercept Models
#'
#' This learner provides fitting procedures for intercept models. Such models
#' predict the outcome variable simply as the mean of the outcome vector.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.count is.flag
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
#'   - \code{...}: Not used.
#'
#' @examples
#' data(cpp_imputed)
#' covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
#' task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")
#'
#' # simple, main-terms GLM
#' lrnr_mean <- make_learner(Lrnr_mean)
#' mean_fit <- lrnr_mean$train(task)
#' mean_preds <- mean_fit$predict()
Lrnr_mean <- R6Class(
  classname = "Lrnr_mean",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(...) {
      params <- list(...)
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial", "categorical", "weights", "offset"),
    .train = function(task) {
      outcome_type <- self$get_outcome_type(task)
      y <- outcome_type$format(task$Y)
      weights <- task$weights

      if (task$has_node("offset")) {
        offset <- task$offset
        if (outcome_type$type == "categorical") {
          # todo: fix
          stop("offsets not yet supported for outcome_type='categorical'")
        }
      } else {
        offset <- rep(0, task$nrow)
      }

      if (outcome_type$type == "categorical") {
        y_levels <- outcome_type$levels
        means <- sapply(
          y_levels,
          function(level) weighted.mean(y == level, weights)
        )
        named_matrix <- matrix(means, nrow = 1)
        colnames(named_matrix) <- y_levels
        fit_object <- list(mean = pack_predictions(named_matrix))
      } else {
        fit_object <- list(mean = weighted.mean(y - offset, weights))
      }

      fit_object$training_offset <- task$has_node("offset")

      return(fit_object)
    },
    .predict = function(task = NULL) {
      predictions <- rep(private$.fit_object$mean, task$nrow)

      if (self$fit_object$training_offset) {
        offset <- task$offset_transformed(NULL, for_prediction = TRUE)
        predictions <- predictions + offset
      }

      predictions <- as.matrix(predictions, ncol = 1)
      return(predictions)
    }
  )
)
