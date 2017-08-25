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
                         if(length(learners)==1&&inherits(learners,"Stack")){
                           return(learners)
                         }
                         params=list(learners=learners)
                         # underlying learners can choose to memoise or not
                         super$initialize(params=params, memoise_learner = FALSE) 
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
                         learner_fits = private$.fit_object$learner_fits
                         learners=self$params$learners
                         learner_names=sapply(learners,function(learner)learner$name)
                         n_to_pred=nrow(task$X)
                         n_learners=length(learner_names)

                         ## Cannot use := to add columns to a null data.table (no columns),
                         ## hence we have to first seed an initial column, then delete it later
                         learner_preds = data.table::data.table(init_seed_preds_to_delete = rep(NA_real_,n_to_pred))

                         #todo: should be foreach or future_lapply
                         for(i in seq_along(learner_fits)) {
                           current_fit=learner_fits[[i]]
                           current_preds = current_fit$predict(task)
                           current_names = learner_names[i]
                           if (!is.na(safe_dim(current_preds)[2]) && safe_dim(current_preds)[2] > 1) {
                            current_names = paste0(learner_names[i], "_", names(current_preds))
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

