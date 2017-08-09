#not only prediction, but also basis functions, variable selection, data cleaning, etc
#this object is cloned and .fit_object is populated when train is called
#todo: I think in general we want to wrap the train in prediction methods for subclasses with consistent sanity checks and error handling
#' @importFrom assertthat assert_that is.count is.flag
#' @export
Learner <- R6Class(classname = "Learner",
                   portable = TRUE,
                   class = TRUE,
                   public = list(
                     params = NULL,
                     initialize = function(...) {
                       params=list(...)
                       if(length(params)==1 && names(params)=="params"){
                         params=params$params
                       }

                       self$params=params

                       invisible(self)
                     },
                     train = function(task) {
                        #trains learner to data
                        assert_that(is(task,"Learner_Task"))

                        ##OS 08/08/17: Over-write task covariates (if specified for this learner as part of 'params'). Otherwise copy covariates from task.
                        ##todo: add checks for errors, params$covariates must be alway a subset of task$nodes$covariates
                        # covariates <- task$nodes$covariates
                        # if ("covariates" %in% names(self$params)) {
                        #   self$params$covariates <- intersect(covariates, self$params$covariates)
                        # } else {
                        #   self$params$covariates <- covariates
                        # }

                        #todo: add error handling
                        fit_object = private$.train(task)

                        new_object=self$clone() # copy parameters, and whatever else
                        new_object$set_train(fit_object, task)

                        return(new_object)
                     },

                     set_train = function(fit_object, training_task){
                       #todo: figure out how to do this without a public method that is mutuating private variables.
                       private$.fit_object = fit_object
                       private$.training_task = training_task


                     },

                     predict = function(task = NULL){
                       if(!self$is_trained){
                         stop("Learner has not yet been train to data. Call learner$train(task) first.")
                       }

                       if(is.null(task)){
                         task <- private$.training_task
                       }
                       assert_that(is(task,"Learner_Task"))

                       predictions = private$.predict(task)

                       return(predictions)
                     },

                    chain = function(task = NULL){
                      if(!self$is_trained){
                        stop("Learner has not yet been train to data. Call learner$train(task) first.")
                      }

                      if(is.null(task)){
                        task <- private$.training_task
                      }
                      assert_that(is(task,"Learner_Task"))

                      next_task = private$.chain(task)

                      return(next_task)


                     },
                    print = function(){
                      print(self$name)
                      #print(self$params)
                      fit_object=private$.fit_object
                      if(!is.null(fit_object))
                      print(fit_object)
                    }),
                   active = list(
                     is_trained=function(){
                       return(!is.null(private$.fit_object))
                     },
                     name=function(){
                       #todo: allow custom names
                       if(is.null(private$.name)){

                         params=self$params
                         if(length(params)>0){
                           #todo: sanitize further
                           atom_params=sapply(params,is.atomic)
                           params=params[atom_params]
                         }
                         props = c(list(class(self)[1]), params)
                         name = paste(props,collapse="_")
                         private$.name = name
                       }

                       return(private$.name)
                     }
                   ),
                   private = list(
                     .name = NULL,
                     .fit_object = NULL,
                     .training_task = NULL,
                     .train = function(task){
                       stop("Learner is meant to be abstract, you should instead use specific learners. See listLearners()")
                     },
                     .predict = function(task){
                       predictions = predict(private$.fit_object, newdata=task$X)
                       return(predictions)
                     },
                     .chain = function(task){
                       predictions = self$predict(task)
                       predictions = as.data.table(predictions)
                       colnames(predictions)= paste(self$name,colnames(predictions), sep="_")

                       #now make a new task where these are the covariates

                       return(task$next_in_chain(predictions))
                     }
                   )
)

#todo: implement predict S3 method


print.Learner=function(object){
  object$print()
}