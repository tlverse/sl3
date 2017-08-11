#' Learner Stacking
#' A Stack is a special Learner that combines multiple other learners, "stacking" their predictions in columns. Currently, train fits the learners one-at-a-time, but this will be parallelized with \link{future} going forward.
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return \code{\link{Lrnr_base}} object with methods for training and prediction
#' @format \code{\link{R6Class}} object.
#' @field params A list of learners to stack
#' @section Methods:
#' \describe{
#'   \item{\code{new(...)}}{This method is used to create a stack of learners. Arguments should be indiviual \code{Learner}s.}
#'   }
#' @importFrom assertthat assert_that is.count is.flag
#' @family Learners
Stack <- R6Class(classname = "Stack",
                     inherit= Lrnr_base,
                     portable = TRUE,
                     class = TRUE,
                     public = list(
                       initialize = function(...) {
                         learners=list(...)
                         params=list(learners=learners)
                         super$initialize(params=params)
                       },
                       print = function(){
                         if(is.null(private$.fit_object)){
                           lapply(self$params$learners,print)
                         } else {
                           lapply(private$.fit_object,print)
                         }
                       }),
                     active = list(
                       name = function(){
                         learners=self$params$learners
                         learner_names=sapply(learners,function(learner)learner$name)
                         name = paste(learner_names, collapse="x")
                         
                         return(name)
                       }
                     ),
                     private = list(
                       .train = function(task) {
                         # browser()
                         learners=self$params$learners
                         learner_names=sapply(learners,function(learner)learner$name)
                         learner_fits=as.list(rep(NA,length(learners)))
                         names(learner_fits)=learner_names
                         #todo: should be foreach or future_lapply
                         for(i in seq_along(learners)){
                           current_learner=learners[[i]]
                           fit = current_learner$train(task)
                           learner_fits[[i]]=fit
                        }
                        
                         fit_object <- list(learner_fits = learner_fits)
                         
                         return(fit_object)
                         
                       },
                       .predict = function(task){
                         # browser()
                         learner_fits = private$.fit_object$learner_fits
                         learners=self$params$learners
                         learner_names=sapply(learners,function(learner)learner$name)
                         n_to_pred=nrow(task$X)
                         n_learners=length(learner_names)
                         learner_preds=matrix(NA, nrow = n_to_pred, ncol=n_learners)
                         #todo: should be foreach or future_lapply
                         for(i in seq_along(learner_fits)){
                           current_fit=learner_fits[[i]]
                           learner_preds[,i]=current_fit$predict(task)
                         }
                         
                         colnames(learner_preds)=learner_names
                         
                         return(learner_preds)
                       },
                       .chain = function(task){
                         predictions = self$predict(task)
                         predictions = as.data.table(predictions)
                         
                         #now make a new task where these are the covariates
                         
                         return(task$next_in_chain(predictions))
                       }
                       
                       )
                     
)

