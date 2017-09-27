
#' @importFrom assertthat assert_that is.count is.flag
#' @import origami
#' @export
#' @rdname undocumented_learner
Lrnr_sl <- R6Class(classname = "Lrnr_sl",
                   inherit= Lrnr_base,
                   portable = TRUE,
                   class = TRUE,
                   public = list(
                     initialize = function(learners, metalearner, folds = NULL, ...) {
                       #kludge to deal with stack as learners
                       if(inherits(learners,"Stack")){
                         learners <- learners$params$learners
                       }
                       
                       params=list(learners=learners, metalearner=metalearner, folds=folds, ...)
                       super$initialize(params=params, ...)
                     },
                     print = function(){
                       print("SuperLearner")
                       print(self$params$learners)
                       
                       if(self$is_trained){
                         print(private$.fit_object)
                       }
                     },
                     metalearner_fit = function(){
                       self$assert_trained()
                       return(private$.fit_object$cv_meta_fit$fit_object)
                     },
                     cv_risk = function(loss_fun){
                       warning("cv_risks are for demonstration purposes only. Don't trust these for now")
                       cv_meta_task <- self$fit_object$cv_meta_task
                       cv_meta_fit <- self$fit_object$cv_meta_fit
                       losses <- cv_meta_task$X[,lapply(.SD,loss_fun,cv_meta_task$Y)]
                       losses[, SuperLearner:=loss_fun(cv_meta_fit$predict(),cv_meta_task$Y)]
                       
                       #get fold specific risks
                       validation_means <- function(fold, losses,weight){
                         risks <- lapply(validation(losses), weighted.mean, validation(weight))
                         return(list(risks=as.data.frame(risks)))
                       }
                       
                       #todo: this ignores weights, square errors are also incorrect
                       fold_risks <- cross_validate(validation_means, cv_meta_task$folds, losses, cv_meta_task$weights)$risks
                       mean_risks <- apply(fold_risks,2,mean)
                       max_risks <- apply(fold_risks,2,max)
                       min_risks <- apply(fold_risks,2,min)
                       se <- apply(fold_risks, 2, sd)
                       

                       learner_names <- c(sapply(learners, "[[","name"),"SuperLearner")
                       risk_dt <- data.table::data.table(learner=learner_names, mean=mean_risks, se=se, min=min_risks,max=max_risks)
                       
                       return(risk_dt)
                     }
                   ),
                   active = list(
                     name = function(){
                       name = paste("CV",self$params$learner$name,sep="_")
                     }
                   ),
                   private = list(
                     .pretrain = function(task){
                       #prefer folds from params, but default to folds from task
                       folds <- self$params$folds
                       if(is.null(folds)){
                         #todo: this breaks if task is delayed
                         folds <- task$folds
                       }
                       
                       #make stack and cv learner objects
                       learners <- self$params$learners
                       learner_stack <- do.call(Stack$new,learners)
                       cv_stack <- Lrnr_cv$new(learner_stack, folds=folds)
                       
                       #fit stack on cv data
                       cv_fit <- delayed_learner_train(cv_stack, task)
                       
                       #fit metalearner
                       metalearner <- self$params$metalearner
                       cv_meta_task <- delayed_learner_fit_chain(cv_fit,task)
                       cv_meta_fit <- delayed_learner_train(metalearner, cv_meta_task)
                       
                       #refit stack on full data
                       stack_fit <- delayed_learner_train(learner_stack, task)
                       
                       #form full SL fit -- a pipeline with the stack fit to the full data,
                       #and the metalearner fit to the cv predictions
                       full_fit <- delayed_learner_new(Pipeline,stack_fit,cv_meta_fit)
                       
                       
                       fit_object <- list(cv_meta_task=cv_meta_task, cv_meta_fit=cv_meta_fit, full_fit=full_fit)
                       return(bundle_delayed(fit_object))

                     },
                     .train = function(task, pretrain) {
                       
                       return(pretrain)
                     },
                     
                     .predict = function(task){
                       predictions=private$.fit_object$full_fit$predict(task)
                       return(predictions)
                     }
                     
                   )
                   
)

