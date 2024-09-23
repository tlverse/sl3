#' Augmented Covariate Screener
#'
#' This learner augments a set of screened covariates with covariates that
#' should be included by default, even if the screener did not select them.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#'
#' @export
#'
#' @keywords data
#'
#' @return \code{\link[sl3]{Lrnr_base}} object with methods for training and
#'  prediction
#' @format \code{\link[R6]{R6Class}} object.
#'
#' @family Learners
#'
#' @section Parameters:
#' \describe{
#'   \item{\code{screener}}{An instantiated screener.}
#'   \item{\code{default_covariates}}{Vector of covariate names to be
#'   automatically added to the vector selected by the screener, regardless of
#'   whether or not these covariates were selected by the screener.}
#'   \item{\code{...}}{Other parameters passed to \code{screener}.}
#' }
#'
#' @examples
#' library(data.table)
#'
#' # load example data
#' data(cpp_imputed)
#' setDT(cpp_imputed)
#' cpp_imputed[, parity_cat := factor(ifelse(parity < 4, parity, 4))]
#' covars <- c(
#'   "apgar1", "apgar5", "parity_cat", "gagebrth", "mage", "meducyrs",
#'   "sexn"
#' )
#' outcome <- "haz"
#'
#' # create sl3 task
#' task <- sl3_Task$new(data.table::copy(cpp_imputed),
#'   covariates = covars,
#'   outcome = outcome
#' )
#'
#' screener_cor <- make_learner(
#'   Lrnr_screener_correlation,
#'   type = "rank",
#'   num_screen = 2
#' )
#' screener_augment <- Lrnr_screener_augment$new(screener_cor, covars)
#' screener_fit <- screener_augment$train(task)
#' selected <- screener_fit$fit_object$selected
#' screener_selected <- screener_fit$fit_object$screener_selected
Lrnr_screener_augment <- R6Class(
  classname = "Lrnr_screener_augment",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(screener, default_covariates, ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("screener"),
    .train = function(task) {
      screener <- self$params$screener
      screener_fit <- screener$train(task)
      screener_selected <- screener_fit$fit_object$selected
      selected <- unique(c(self$params$default_covariates, screener_selected))

      fit_object <- list(
        selected = selected,
        default_covariates = self$params$default_covariates,
        screener_selected = screener_selected
      )
      return(fit_object)
    },
    .predict = function(task) {
      task$data[, private$.fit_object$selected, with = FALSE, drop = FALSE]
    },
    .chain = function(task) {
      return(task$next_in_chain(covariates = private$.fit_object$selected))
    },
    .required_packages = c()
  )
)
