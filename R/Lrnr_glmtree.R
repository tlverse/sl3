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
    # list properties your learner supports here.
    # Use sl3_list_properties() for a list of options
    .properties = c("continuous", "binomial", "weights", "offset"),

    # list any packages required for your learner here.
    .required_packages = c("partykit"),

    # .train takes task data and returns a fit object that can be used to generate predictions
    .train = function(task) {
      # generate an argument list from the parameters that were
      # captured when your learner was initialized.
      # this allows users to pass arguments directly to your ml function
      args <- self$params

      # get outcome variable type
      # preferring learner$params$outcome_type first, then task$outcome_type
      outcome_type <- self$get_outcome_type(task)
      if (is.null(args$family)) {
        args$family <- outcome_type$glm_family(return_object = TRUE)
      }
      # family_name <- args$family$family
      # linkinv_fun <- args$family$linkinv
      # link_fun <- args$family$linkfun
      # should pass something on to your learner indicating outcome_type
      # e.g. family or objective

      # add task data to the argument list
      # what these arguments are called depends on the learner you are wrapping
      # args$x <- as.matrix(task$X_intercept)
      # args$y <- outcome_type$format(task$Y)

      args$formula <- as.formula(paste(task$nodes$outcome, paste(task$nodes$covariates, collapse = "+"), sep = "~"))
      args$data <- task$data
      # args$formula <- paste(task$Y, task$X, sep = "~" )

      # only add arguments on weights and offset
      # if those were specified when the task was generated
      if (task$has_node("weights")) {
        args$weights <- task$weights
      }else{
        args$weights <-NULL
        
      }

      if (task$has_node("offset")) {
        offset <- task$offset
      }else{
        offset <- NULL
      }

      # call a function that fits your algorithm
      # with the argument list you constructed

      fit_object <- partykit::glmtree(formula = args$formula,
                                      family = args$family,
                                      data = args$data, 
                                      weights = args$weights,
                                      offset = offset,
                                      alpha = args$alpha, 
                                      maxdepth = args$maxdepth,
                                      prune = args$prune)
                                      
                                      

      # return the fit object, which will be stored
      # in a learner object and returned from the call
      # to learner$predict
      return(fit_object)
    },

    # .predict takes a task and returns predictions from that task
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
