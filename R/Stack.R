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
                         if(length(learners)==1){
                           if(inherits(learners[[1]],"Stack")){
                             #if we were passed a stack (instead of learners as separate parameters), just copy that stack's learners
                             learners <- learners[[1]]$params$learners
                           } else if(is.list(learners[[1]])){
                             #if we were passed a list of learners (instead of learners as separate parameters), use that list
                             learners <- learners[[1]]
                           }
                         }
                         
                         learner_names <- sapply(learners, `[[`, "name")
                         if(any(duplicated(learner_names))){
                           warning("Stack has learners with identical names. This is unsupported and might lead to errors")
                         }
                         params=list(learners=learners)
                         super$initialize(params=params) 
                       },
                       print = function(){
                         if(is.null(private$.fit_object)){
                           lapply(self$params$learners,print)
                         } else {
                           lapply(private$.fit_object,print)
                         }
                       },
                       update_errors = function(is_error){
                         private$.fit_object$is_error <- is_error
                       }
                     ),
                     active = list(
                       name = function(){
                         # learners=self$params$learners
                         # learner_names=sapply(learners,function(learner)learner$name)
                         # name = paste(learner_names, collapse="x")
                         name = "Stack"
                         return(name)
                       }
                     ),
                     private = list(
                       .pretrain = function(task){
                         #generate training subtasks
                         learners <- self$params$learners
                         subtasks <- lapply(learners, function(learner){
                           delayed_learner <- delayed_learner_train(learner, task)
                           delayed_learner$expect_error <- TRUE
                           
                           return(delayed_learner)
                         })
                         
                         return(bundle_delayed(subtasks))
                       },
                       .train = function(task, pretrain) {
                         
                         #check fits for errors
                         is_error <- sapply(pretrain, function(result){inherits(result,"error")||inherits(result,"try-error")})
                         learner_errors <- pretrain[is_error]
                         errored_learners <- self$params$learners[is_error]
                         
                         for(i in seq_along(errored_learners)){
                           message <- learner_errors[[i]]$message
                           learner <- errored_learners[[i]]
                           warning(sprintf("%s failed with message: %s. It will be removed from the stack", learner$name, message))
                         }
                         
                         if(all(is_error)){
                           stop("All learners in stack have failed")
                         }
                         fit_object <- list(learner_fits = pretrain, learner_errors = learner_errors, is_error = is_error)

                         return(fit_object)

                       },
                       .predict = function(task){
                         is_error <- private$.fit_object$is_error
                         learner_fits <- private$.fit_object$learner_fits[!is_error]
                         learners <- self$params$learners[!is_error]
                         learner_names <- sapply(learners,function(learner)learner$name)
                         n_to_pred <- nrow(task$X)
                         n_learners <- length(learner_names)

                         ## Cannot use := to add columns to a null data.table (no columns),
                         ## hence we have to first seed an initial column, then delete it later
                         learner_preds <- data.table::data.table(init_seed_preds_to_delete = rep(NA_real_,n_to_pred))

                         for(i in seq_along(learner_fits)) {
                           current_fit  <- learner_fits[[i]]
                           current_preds <- current_fit$predict(task)
                           current_names <- learner_names[i]
                           if (!is.na(safe_dim(current_preds)[2]) && safe_dim(current_preds)[2] > 1) {
                            current_names <- paste0(learner_names[i], "_", names(current_preds))
                            stopifnot(length(current_names) == safe_dim(current_preds)[2])
                           }
                           data.table::set(learner_preds, j = current_names, value = current_preds)
                           invisible(NULL)
                         }

                         ## remove the initial seeded column by reference
                         data.table::set(learner_preds, j = "init_seed_preds_to_delete", value = NULL)
                         return(learner_preds)
                       }
                       )

)

