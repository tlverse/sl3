##' Template of a \code{sl3} Learner.
##'
##' This learner uses \code{\link[svm]{svm.train}} from \code{svm} to fit my favorite machine learning algorithm.
##' 
##' @docType class
##' @importFrom R6 R6Class
##' @export
##' @keywords data
##' @return Learner object with methods for training and prediction. See \code{\link{Lrnr_base}} for documentation on learners.
##' @format \code{\link{R6Class}} object.
##' @family Learners
##' 
##' @section Parameters:
##' \describe{
##'   \item{\code{nu="nu"}}{Type of machine}
##'   \item{\code{param_2="default_2"}}{ This parameter does something else.
##'   }
##'   \item{\code{...}}{ Other parameters passed directly to \code{\link[svm]{svm.train}}. See its documentation for details.
##'   }
##' }
##' 
##' @section Methods:
##' \describe{
##' \item{\code{special_function(arg_1)}}{
##'   My learner is special so it has a special function.
##'   
##'   \itemize{
##'     \item{\code{arg_1}: A very special argument.
##'    }
##'   }
##'   }
##' }
Lrnr_svm <- R6Class(classname = "Lrnr_svm", inherit = Lrnr_base,
                    portable = TRUE, class = TRUE,
# Above, you should change Lrnr_template (in both the object name and the classname argument)
# to a name that indicates what your learner does
  public = list(
    
    # you can define default parameter values here
    # if possible, your learner should define defaults for all required parameters
    initialize = function(family, 
                          type = "nu-regression",
                          kernel = "radial",
                          nu = 0.5,
                          degree = 3,
                          cost = 1,
                          coef0 = 0,
                          fitted = FALSE,
                          probability = TRUE, ...) {
      # this captures all parameters to initialize and saves them as self$params  
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
    # you can define public functions that allow your learner to do special things here
    # for instance glm learner might return prediction standard errors
  #   special_function = function(arg_1){
  #     
  #   }
  # ),

  private = list(
    # list properties your learner supports here. 
    # Use sl3_list_properties() for a list of options
    .properties = c("continuous", "binomial", "categorical"),
    
    # list any packages required for your learner here.
    .required_packages = c("e1071"),
    
    # .train takes task data and returns a fit object that can be used to generate predictions
    .train = function(task) {
      # generate an argument list from the parameters that were
      # captured when your learner was initialized.
      # this allows users to pass arguments directly to your ml function
      args <- self$params
      
      
      # get outcome variable type
      # prefering learner$params$outcome_type first, then task$outcome_type
      outcome_type <- self$get_outcome_type(task)
      # should pass something on to your learner indicating outcome_type
      # e.g. family or objective
      
      # add task data to the argument list
      # what these arguments are called depends on the learner you are wrapping
      args$x <- as.matrix(task$X)
      args$y <- outcome_type$format(task$Y)
      
      # only add arguments on weights and offset 
      # if those were specified when the task was generated
      # if(task$has_node("weights")){
      #   args$weights <- task$weights
      # }
      # 
      # if(task$has_node("offset")){
      #   args$offset <- task$offset
      # }
      
      # call a function that fits your algorithm
      # with the argument list you constructed
      fit_object <- call_with_args(my_ml_fun, args)
      
      # return the fit object, which will be stored
      # in a learner object and returned from the call
      # to learner$predict
      svm_fit <- getS3method(
        "svm", "default",
        envir = getNamespace("svm")
      )
      fit_object <- call_with_args(svm_fit, args)
      return(fit_object)
    },
    
    # .predict takes a task and returns predictions from that task
    .predict = function(task = NULL) {
      self$training_task
      self$training_outcome_type
      self$fit_object
      
      predictions <- predict(self$fit_object, task$X)
      return(predictions)
    }
  ) ,
)

