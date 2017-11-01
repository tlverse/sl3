#' Classification from Binomial Regression
#'
#' This learner provides converts a binomial learner into a multinomial learner using a series of independent binomials. 
#' The procedure is modeled on \url{https://en.wikipedia.org/wiki/Multinomial_logistic_regression#As_a_set_of_independent_binary_regressions}
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return Learner object with methods for training and prediction. See \code{\link{Lrnr_base}} for documentation on learners.
#' @format \code{\link{R6Class}} object.
#' @family Learners
#' 
#' @section Parameters:
#' \describe{
#'   \item{\code{binomial_learner}}{The learner to wrap}
#' }
#' @template common_parameters
#' @importFrom assertthat assert_that is.count is.flag
Lrnr_independent_binomial <- R6Class(classname = "Lrnr_independent_binomial",
                             inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(binomial_learner = NULL, ...) {
      if(is.null(binomial_learner)){
        binomial_learner <- make_learner(Lrnr_glm_fast)
      }
      params = list(binomial_learner=binomial_learner, ...)
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("categorical"),
    .train = function(task) {
      
      outcome_type <- self$get_outcome_type(task)
      if(outcome_type$type != "categorical"){
        stop("Lrnr_independent_binomial only works for categorical outcomes")
      }
      X <- as.matrix(task$X)
      Y <- outcome_type$format(task$Y)
      Y_levels <- levels(Y)
      
      reference <- Y_levels[1]
      others <- Y_levels[-1]

      binomial_learner <- self$params$binomial_learner
      
      #define bintask outcome as indicator of not being in the reference group
      column_names <- task$add_columns(self$uuid, data.table(binary_outcome = as.numeric(Y!=reference)))
      bintask <- task$next_in_chain(outcome="binary_outcome", column_names = column_names)

      fit_object <- list()
      for(Y_level in others){
        # subset task to data in this category and reference
        sub_task <- bintask[Y %in% c(Y_level, reference)]
        fit_object[[Y_level]] <- binomial_learner$train(sub_task)
        
      }
      
      
      return(fit_object)
    },
    .predict = function(task) {
      
      
      raw_preds <- lapply(self$fit_object, learner_fit_predict, task)
      raw_preds <- as.matrix(do.call(cbind, raw_preds))
      
      # transform on exponential scale
      transformed <- exp(qlogis(raw_preds))
      
      # compute baseline
      baseline <- 1/(1+rowSums(transformed))
      # compute other categories relative to baseline
      predictions <- baseline * transformed
      predictions <- cbind(baseline, predictions)
      
      predictions <- pack_predictions(predictions)
      
      return(predictions)
    },
    .required_packages = c("randomForest")
  )
)

