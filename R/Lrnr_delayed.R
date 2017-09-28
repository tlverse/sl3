#todo: environment issue with r6 methods -- r6 class methods come with an environment that isn't respected by delayed_fun. 
#Therefore, there are environment-less wrapper functions here. We should be able to eliminate the need for these.

#' Fit/Predict a learner delayed
#' A wrapper around any learner that delayes computation
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return \code{\link{Lrnr_base}} object with methods for training and prediction
#' @format \code{\link{R6Class}} object.
#' @field params A learner to be wrapped
#' @section Methods:
#' \describe{
#'   \item{\code{new(learner)}}{Wraps the \code{learner} in a delayed computation}
#'   }
#' @importFrom assertthat assert_that is.count is.flag
#' @family Learners
Lrnr_delayed <- R6Class(classname = "Lrnr_delayed",
                   inherit= Lrnr_base,
                   portable = TRUE,
                   class = TRUE,
                   public = list(
                     initialize = function(learner, ...) {
                       params <- list(learner=learner, ...)
                       super$initialize(params=params, ...)
                     },
                     print = function(){
                       print("Lrnr_delayed")
                       print(self$params$learner)
                       #todo: check if fit
                     }),
                   active = list(
                     name = function(){
                       name = paste("delayed",self$params$learner$name,sep="_")
                     }
                   ),
                   private = list(
                     .train = function(task) {
                       learner <- self$params$learner
                       train_learner <- function(learner, task){
                         learner$train(task)
                       }
                       
                       return(list(learner_fit=delayed_fun(train_learner)(learner,task)))
                     },
                     
                     .predict = function(task){
                       learner_fit <- private$.fit_object$learner_fit
                       predict_task <- function(learner_fit, task){
                         learner_fit$predict(task)
                       }
                       
                       return(delayed_fun(predict_task)(learner_fit,task))
                     },
                     
                     .chain = function(task){
                       learner_fit <- private$.fit_object$learner_fit
                       chain_task <- function(learner_fit, task){
                         learner_fit$chain(task)
                       }
                       
                       return(delayed_fun(chain_task)(learner_fit,task))
                       
                     },
                     .required_packages = c("delayed")
                   )
                   
)

