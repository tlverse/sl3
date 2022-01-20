#' Learner for GLM tree.
#'
#' This learner uses \code{\link[glmtree]{glmtree}} from \code{partykit} to fit
#' recursive partitioning and regression trees in a general linear model.
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
#' \describe{
#'   \item{\code{model}}{If logical: keep a copy of the model frame in the
#'     result?
#'   }
#'   \item{\code{x}}{Whether to keep a copy of the x matrix in the result.
#'   }
#'   \item{\code{y}}{Whether to keep a copy of the dependent variable in the
#'     result.
#'   }
#'   \item{\code{...}}{ Other parameters to be passed directly to
#'     \code{\link[glmtree]{partykit}}. See its documentation for details.
#'   }
#' }
#
Lrnr_glmtree <- R6Class(
  classname = "Lrnr_glmtree", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  # Above, you should change Lrnr_template (in both the object name and the classname argument)
  # to a name that indicates what your learner does
  public = list(
    # you can define default parameter values here
    # if possible, your learner should define defaults for all required parameters
    initialize = function(formula = NULL,
                          family = NULL,
                          alpha = 0.05,
                          maxdepth = 10,
                          prune = NULL) {
      # this captures all parameters to initialize and saves them as self$params
      params <- args_to_list()
      super$initialize(params = params)
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
