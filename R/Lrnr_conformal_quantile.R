#' Conformalized Quantile Regression
#'
#' This learner implements the Conformalized Quantile Regression algorithm, which
#' generates Prediction Intervals based on Conformal Inference.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stats predict
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @export
#'
#' @keywords data
#'
#' @return Learner object with methods for training and prediction. See
#'   \code{\link{Lrnr_base}} for documentation on learners.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @family Learners
#'
#' @section Parameters:
#' \describe{
#'   \item{\code{alpha_upper}}{Upper alpha value, with a default value of 0.975.}
#'   \item{\code{alpha_lower}}{Lower alpha value, with a default value of 0.025.}
#'   \item{\code{task}}{\code{sl3} task object}
#'   \item{\code{fit=NULL}}{Trained \code{sl3} object for the quantile regression. 
#'   If \code{fit=NULL}, half of the data provided in the \code{task} will be used to 
#'   generate a fit.}
#' }
#'
#' @examples
#' 
#' #Get the data and make sl3 tasks:
#' data(cpp_imputed)
#' covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
#' task_train <- sl3_Task$new(cpp_imputed[1:100,], covariates = covs, outcome = "haz")
#' task_test <- sl3_Task$new(cpp_imputed[200:300,], covariates = covs, outcome = "haz")
#'
#' #Canonical Split-Conformal Inference
#' lrnr_conf <- Lrnr_conformal_quantile$new(task=task_train)
#' lrnr_conf$train()
#' pred_interval <- lrnr_conf$predict(task=task_test)
#' 
#' 
Lrnr_conformal_quantile <- R6Class(
  classname = "Lrnr_conformal_quantile",
  inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(alpha_upper = 0.975, alpha_lower=0.025, 
                          task, fit=NULL, ...) {
      
      #If no fit passes, fit quantile regression here
      if(is.null(fit)){
        warning("Half of the data provided will be used for generating the initial fit.")
        
        #Fit on half the samples, as canonical version
        folds <- origami::make_folds(n=nrow(task$data),V=2)
        
        task_train <- task$subset_task(folds[[1]]$training_set)
        task_test  <- task$subset_task(folds[[1]]$validation_set)
        
        lrnr_qf <- Lrnr_quantregForest$new(alpha = c(alpha_lower,alpha_upper))
        fit <- lrnr_qf$train(task_train)
        
        task <- task_test
      }
      
      params <- list(
        alpha_upper=alpha_upper,
        alpha_lower=alpha_lower,
        task=task, fit=fit,  ...
      )
      super$initialize(params = params, ...)
      
      #Generate upper and lower quantile values
      q <- fit$predict(task)
      private$.q_lower <- q[,1]
      private$.q_upper <- q[,2]
      private$.fit <- fit
    }, 
    
    train = function(){
      
      alpha_lower <- self$params$alpha_lower
      alpha_upper <- self$params$alpha_upper
      
      #Get the predicted quantiles:
      q_lower <- private$.q_lower
      q_upper <- private$.q_upper
      
      #compute the conformity score:
      conf_score <- self$cqr_score(q_lower, q_upper,
                                   task = self$params$task)
      
      #Take the conformity scores, and apply the correction:
      conf_correct <- self$cqr_correction(conf_score, alpha_lower)
      
      #Save the correction and conformity score
      private$.conf_score <- conf_score
      private$.conf_correct <- conf_correct
    }, 
    
    cqr_score = function(q_lower, q_upper, task){
      Y <- task$Y
      
      err_low <- q_lower - Y
      err_high <- Y - q_upper
      confScore <- cbind.data.frame(err_low, err_high)
      
      conf_score <- apply(confScore, 1, max)
      return(conf_score)
    },
    
    cqr_correction = function(conf_score, alpha){
      return( quantile(conf_score, min(1, (1-alpha)*(1+1/length(conf_score)))) )
    },
    
    predict = function(task){
      
      alpha_lower <- self$params$alpha_lower
      alpha_upper <- self$params$alpha_upper
      
      #Get the quantile preds for the new task
      fit <- private$.fit
      q <- fit$predict(task)
      q_lower <- q[,1]
      q_upper <- q[,2]
      
      #Get the correction:
      conf_correct <- private$.conf_correct
      
      #Compute quantiles:
      lower <- q_lower - conf_correct
      upper <- q_upper + conf_correct
      PI <- cbind.data.frame(lower, upper)
      names(PI) <- c(paste0(alpha_lower*100, " Quantile"),
                     paste0(alpha_upper*100, " Quantile"))
      
      return(Prediction_Interval=PI)
    }
  ),
  private = list(
    .properties = c("Prediction Interval"),
    .fit = NULL,
    .q_lower = NULL, 
    .q_upper = NULL,
    .conf_score = NULL,
    .conf_correct = NULL,
    
    .required_packages = c()
  )
) 