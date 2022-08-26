#' \code{Lrnr_pkg_SuperLearner_screener} -- Interface for \code{SuperLearner}
#' screening algorithms.
#'
#' Use \code{SuperLearner::listWrappers("screen")} for a list of options.
#'
#' @rdname SuperLearner_interface
#'
#' @export
#' 
#' @examples 
#' library(data.table)
#' library(origami)
#' library(SuperLearner)
#' 
#' set.seed(1)
#' 
#' # Load data
#' data(cpp_imputed)
#' 
#' # Make a factor covariate
#' setDT(cpp_imputed)
#' cpp_imputed[, parity_cat := factor(ifelse(parity < 4, parity, 4))]
#' levels(cpp_imputed$parity_cat) <- c("0", "bad level 1", "bad.2", "also_bad", "4+")
#' covars <- c("apgar1", "apgar5", "parity_cat", "gagebrth", "mage", "meducyrs", "sexn")
#' outcome <- "haz"
#' 
#' # Create sl3 Task
#' task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)
#' 
#' # Example of learner chaining
#' slscreener <- Lrnr_pkg_SuperLearner_screener$new("screen.glmnet")
#' glm_learner <- Lrnr_glm$new()
#' screen_and_glm <- Pipeline$new(slscreener, glm_learner)
#' SL.glmnet_learner <- Lrnr_pkg_SuperLearner$new(SL_wrapper = "SL.glmnet")
#' sg_fit <- screen_and_glm$train(task)
#' sg_pred <- sg_fit$predict()
Lrnr_pkg_SuperLearner_screener <- R6Class(
  classname =
    "Lrnr_pkg_SuperLearner_screener",
  inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(SL_wrapper, ...) {
      if (SL_wrapper == "All") {
        wrapper_fun <- NULL
      } else {
        wrapper_fun <- get(SL_wrapper)
      }
      params <- list(wrapper_name = SL_wrapper, wrapper_fun = wrapper_fun, ...)
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("binomial", "continuous", "weights", "ids", "wrapper"),
    .train = function(task) {
      args <- self$params
      outcome_type <- self$get_outcome_type(task)

      if (is.null(args$family)) {
        args$family <- outcome_type$glm_family(return_object = TRUE)
      }
      wrapper <- args$wrapper_fun
      x <- task$X
      if (is.null(wrapper)) {
        selected <- task$nodes$covariates
      } else {
        selected <- wrapper(
          task$Y, x,
          family = args$family,
          obsWeights = task$weights, id = task$id
        )
      }
      selected_names <- names(x)[selected]

      covariates <- task$nodes$covariates
      covariate_selected <- sapply(covariates, function(covariate) {
        any(grep(covariate, selected_names))
      })
      fit_object <- list(selected = covariates[covariate_selected])
      return(fit_object)
    },
    .predict = function(task) {
      task$X[, private$.fit_object$selected, with = FALSE, drop = FALSE]
    },
    .chain = function(task) {
      return(task$next_in_chain(covariates = private$.fit_object$selected))
    },
    .required_packages = c("SuperLearner")
  )
)
