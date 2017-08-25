
# dim that works for vectors too
safe_dim <- function(x) {
  d <- dim(x)
  if (is.null(d)) {
    d <- length(x)
  }

  return(d)
}


# sort n-dimensional array (for multinomial/multivariate SL support)
aorder <- function(mat, index, along = 1) {

  dims <- safe_dim(mat)
  args <- ifelse(along == seq_along(dims), "index", "")
  indexer <- paste(c(args, "drop=F"), collapse = ",")
  call <- sprintf("mat[%s]", indexer)
  result <- eval(parse(text = call))

  return(result)
}

#' Fit/Predict a learner with CV
#' A wrapper around any learner that generates cross-validate predictions
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return \code{\link{Lrnr_base}} object with methods for training and prediction
#' @format \code{\link{R6Class}} object.
#' @field params A learner to be wrapped
#' @section Methods:
#' \describe{
#'   \item{\code{new(learner)}}{Wraps the \code{learner} in a cross-validation layer.}
#'   }
#' @importFrom assertthat assert_that is.count is.flag
#' @family Learners
Lrnr_cv <- R6Class(classname = "Lrnr_cv",
                     inherit= Lrnr_base,
                     portable = TRUE,
                     class = TRUE,
                     public = list(
                       initialize = function(learner, folds=NULL) {
                         params=list(learner=learner, folds=folds)
                         super$initialize(params=params)
                       },
                       print = function(){
                         print("Lrnr_cv")
                         print(self$params$learner)
                         #todo: check if fit
                       }),
                     active = list(
                       name = function(){
                         name = paste("CV",self$params$learner$name,sep="_")
                       }
                     ),
                     private = list(
                       .train = function(task) {

                         #prefer folds from params, but default to folds from task
                         folds=self$params$folds
                         if(is.null(folds)){
                           folds=task$folds
                         }

                         learner=self$params$learner

                         cv_train=function(fold,learner,task){
                           training_task=training(task)
                           fit_object=learner$train(training_task)
                           return(list(fold_fit=fit_object))
                         }

                        fold_fits=cross_validate(cv_train,folds,learner,task,.combine=F, future.globals=F)$fold_fit

                        fit_object=list(folds=folds, fold_fits=fold_fits)

                        return(fit_object)
                       },

                       .predict = function(task){
                         if(!identical(task,private$.training_task)){
                           stop("task must match training task for Lrnr_cv")
                         }
                         #doing train and predict like this is stupid, but that's the paradigm (for now!)
                         folds=private$.fit_object$folds
                         fold_fits = private$.fit_object$fold_fits

                         cv_predict=function(fold,fold_fits,task){
                           validation_task=validation(task)
                           index=validation()
                           fit=fold_index(fold_fits)[[1]]
                           predictions=fit$predict(validation_task)
                           list(index=index,predictions=predictions)
                         }

                         fold_predictions=cross_validate(cv_predict,folds,fold_fits,task, future.globals=F)
                         predictions=aorder(fold_predictions$predictions,order(fold_predictions$index))

                         return(predictions)
                       }
                     )

)

