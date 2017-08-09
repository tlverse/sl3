#' Pipeline (chain) of learners. 
#' A Pipeline of learners is a way to "chain" Learners together, where the output of one learner is used as output for the next learner. This can be used for things like screening, two stage machine learning methods, and Super Learning. A pipeline is fit by fitting the first \code{Learner}, calling \code{chain()} to create the next task, which becomes the training data for the next \code{Learner}. Similarly, for prediction, the predictions from the first \code{Learner} become the data to predict on for the next \code{Learner}.
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return \code{\link{Learner}} object with methods for training and prediction
#' @format \code{\link{R6Class}} object.
#' @field params A list of learners to chain.
#' @section Methods:
#' \describe{
#'   \item{\code{new(...)}}{This method is used to create a pipeline of learners. Arguments should be indiviual \code{Learner}s, in the order they should be applied.}
#'   }
#' @importFrom assertthat assert_that is.count is.flag
#' @family Learners
Pipeline <- R6Class(classname = "Pipeline",
                    inherit= Learner,
                    portable = TRUE,
                    class = TRUE,
                    public = list(
                      initialize = function(...) {
                        learners=list(...)
                        params=list(learners=learners)
                        super$initialize(params=params)
                      },
                      prefit_pipeline = function(learners,task){
                        
                        
                        learners_trained=sapply(learners,`[[`,'is_trained')
                        if(!all(learners_trained)){
                          stop('Not all learners are fit')
                        }
                        fit_object=list(learner_fits=learners)
                        new_object=self$clone() # copy parameters, and whatever else
                        
                        new_object$set_train(fit_object, task)
                        return(new_object)
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
                        name = paste(learner_names, collapse="___")
                        
                        return(name)
                      }
                    ),
                    private = list(
                      .train = function(task) {
                        learners=self$params$learners
                        learner_names=sapply(learners,function(learner)learner$name)
                        learner_fits=as.list(rep(NA,length(learners)))
                        names(learner_fits)=learner_names
                        
                        current_task=task
                        for(i in seq_along(learners)){
                          current_learner=learners[[i]]
                          fit = current_learner$train(current_task)
                          next_task=fit$chain(current_task)
                          learner_fits[[i]]=fit
                          
                          current_task=next_task
                        }
                        
                        fit_object <- list(learner_fits = learner_fits)
                        
                        return(fit_object)
                        
                      },
                      .predict = function(task){
                        
                        #prediction is just chaining until you get to the last fit, and then calling predict
                        learner_fits = private$.fit_object$learner_fits
                        next_task=task
                        for(i in seq_along(learner_fits)){
                          current_task=next_task
                          current_fit=learner_fits[[i]]
                          next_task=current_fit$chain(current_task)
                        }
                        
                        # current_task is now the task for the last fit, so we can just do this
                        predictions=current_fit$predict(current_task)
                        
                        return(predictions)
                      }
                      
                    )
                    
)

