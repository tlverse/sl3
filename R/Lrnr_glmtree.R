#' Generalized Linear Model Trees
#'
#' This learner uses \code{\link[partykit]{glmtree}} from \pkg{partykit} to fit
#' recursive partitioning and regression trees in a generalized linear model.
#'
#' @docType class
#' @import sl3
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
#'   - \code{formula}: An optional object of class \code{formula} (or one that 
#'       can be coerced to that class), which a symbolic description of the 
#'       generalized linear model to be fit. If not specified a main terms 
#'       regression model will be supplied, with each covariate included as 
#'       a term. Please consult \code{\link[partykit]{glmtree}} documentation 
#'       for more information on its use of \code{formula}, and for a 
#'       description on \code{formula} syntax consult the details of the 
#'       \code{\link[stats]{glm}} documentation.
#'   - \code{alpha = 0.05}: Numeric significance level with default of 0.05. 
#'       Please consult \code{\link[partykit]{mob_control}} documentation 
#'       for more information.
#'   - \code{...}: Other parameters passed to 
#'       \code{\link[partykit]{mob_control}} or \code{\link[partykit]{glmtree}}
#'       that are not already specified in the \code{\link{sl3_Task}}. See its 
#'       documentation for details.
#'       
#' @examples
#' data(cpp_imputed)
#' # create task for prediction
#' cpp_task <- sl3_Task$new(
#'   data = cpp_imputed,
#'   covariates = c("bmi", "parity", "mage", "sexn"),
#'   outcome = "haz"
#' )
#' # initialization, training, and prediction with the defaults
#' glmtree_lrnr <- Lrnr_glmtree$new()
#' glmtree_fit <- glmtree_lrnr$train(cpp_task)
#' glmtree_preds <- glmtree_fit$predict()
Lrnr_glmtree <- R6Class(
  classname = "Lrnr_glmtree", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  # Above, you should change Lrnr_template (in both the object name and the classname argument)
  # to a name that indicates what your learner does
  public = list(
    # you can define default parameter values here
    # if possible, your learner should define defaults for all required parameters
    initialize = function(formula = NULL,
                          maxdepth = 10,
                          ...) {
      # this captures all parameters to initialize and saves them as self$params
      params <- args_to_list()
      super$initialize(params = params)
    },
    # for learners that take formula as an argument, the function 
    # process_formula that's defined in Lrnr_base needs to be redefined in
    # the learner like below
    process_formula = function(task) {
      return(task)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial", "weights", "offset"),
    
    .required_packages = c("partykit"),

    .train = function(task) {
    
      args <- self$params

      outcome_type <- self$get_outcome_type(task)
      args$family <- outcome_type$glm_family(return_object = TRUE)

      args$formula <- as.formula(paste(task$nodes$outcome, paste(task$nodes$covariates, collapse = "+"), sep = "~"))
      args$data <- task$data
      
      # only add weights and offset arguments if specified in task
      if (task$has_node("weights")) {
        args$weights <- task$weights
      }
      if (task$has_node("offset")) {
        args$offset <- task$offset
      }

      fit_object <- partykit::glmtree(formula = args$formula,
                                      family = args$family,
                                      data = args$data, 
                                      weights = args$weights,
                                      offset = offset,
                                      alpha = args$alpha, 
                                      maxdepth = args$maxdepth,
                                      prune = args$prune)
                                      
      return(fit_object)
    },

    .predict = function(task = NULL) {
      self$training_task
      self$training_outcome_type
      self$fit_object
      
      if (task$has_node("offset")) {
        predictions <- predict(self$fit_object, task$data) + task$offset
      }else{
        predictions <- predict(self$fit_object, task$data) 
      }
     
      return(predictions)
    }
  )
)
