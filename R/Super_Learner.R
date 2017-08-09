
#' @importFrom assertthat assert_that is.count is.flag
#' @import origami
#' @export
#' @rdname undocumented_learner
Super_Learner <- R6Class(classname = "Super_Learner",
                     inherit= Learner,
                     portable = TRUE,
                     class = TRUE,
                     public = list(
                       initialize = function(learners, metalearner) {
                         params=list(learners=learners, metalearner=metalearner)
                         super$initialize(params=params)
                       },
                       print = function(){
                         print("Super_Learner")
                         print(self$params$learners)
                         
                         if(self$is_trained){
                           print(private$.fit_object)
                         }
                       }),
                     active = list(
                       name = function(){
                         name = paste("CV",self$params$learner$name,sep="_")
                       }
                     ),
                     private = list(
                       .train = function(task) {
                         #make stack and cv learner objects
                         learners=self$params$learners
                         learner_stack=do.call(Stack$new,learners)
                         cv_stack=CV_Learner$new(learner_stack)
                         
                         #fit stack on cv data
                         cv_fit=cv_stack$train(task)
                         
                         #fit metalearner
                         metalearner=self$params$metalearner
                         cv_meta_task=cv_fit$chain()
                         cv_meta_fit=metalearner$train(cv_meta_task)
                         
                         #refit stack on full data
                         stack_fit=learner_stack$train(task)                         
                         sl_learner=Pipeline$new(learner_stack,metalearner)
                         full_fit=sl_learner$prefit_pipeline(list(stack_fit,cv_meta_fit),task)
                         
                         #just by mixing and matching the fit objects above

                        fit_object=list(full_fit=full_fit)
                        
                        return(fit_object)
                       },
                       
                       .predict = function(task){
                         predictions=private$.fit_object$full_fit$predict(task)
                         return(predictions)
                       }
                       
                       )
                     
)

