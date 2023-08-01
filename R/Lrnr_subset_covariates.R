#' Learner with Covariate Subsetting
#'
#' This learner provides fitting procedures for subsetting covariates. It is a
#' convenience utility for reducing the number of covariates to be fit.
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
#'   \item{\code{...}}{Not currently used.}
#' }
#'
#' @template common_parameters
#'
#' @examples
#' # load example data
#' data(cpp_imputed)
#' covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
#' outcome <- "haz"
#'
#' # create sl3 task
#' task <- sl3_Task$new(data.table::copy(cpp_imputed),
#'   covariates = covars,
#'   outcome = outcome,
#'   folds = origami::make_folds(cpp_imputed, V = 3)
#' )
#'
#' glm_learner <- Lrnr_glm$new()
#' glmnet_learner <- Lrnr_glmnet$new()
#' subset_apgar <- Lrnr_subset_covariates$new(covariates = c("apgar1", "apgar5"))
#' learners <- list(glm_learner, glmnet_learner, subset_apgar)
#' sl <- make_learner(Lrnr_sl, learners, glm_learner)
#'
#' sl_fit <- sl$train(task)
#' sl_pred <- sl_fit$predict()
Lrnr_subset_covariates <- R6Class(
  classname = "Lrnr_subset_covariates",
  inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(...) {
      params <- list(...)
      super$initialize(...)
    }
  ),
  private = list(
    .train = function(task) {
      fit_object <- list()
      return(fit_object)
    },
    .predict = function(task = NULL) {
      # nothing to do here: we're relying on Lrnr_base to subset covariates
      return(task$X)
    },
    .chain = function(task = NULL) {
      # nothing to do here: we're relying on Lrnr_base to subset covariates
      return(task)
    }
  )
)
