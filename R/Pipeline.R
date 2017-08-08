#Pipeline (chain) of learners. 
#todo: be able to construct a pipeline out of already fit learners (e.g. for SuperLearner)
#' @importFrom assertthat assert_that is.count is.flag
#' @export
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

