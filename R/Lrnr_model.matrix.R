#support for converting data frame with raw columns into a model matrix with things like interaction terms and factor indicators. todo: reimplement this without using model.matrix.
#' @importFrom assertthat assert_that is.count is.flag
#' @export
#' @rdname undocumented_learner
Lrnr_model.matrix <- R6Class(classname = "Lrnr_model.matrix",
                       inherit= Lrnr_base,
                       portable = TRUE,
                       class = TRUE,
                       public = list(
                         initialize = function(formula, ...) {
                           params=list(formula=formula, ...)
                           super$initialize(params=params, ...)
                         }),
                       private = list(
                         .train = function(task) {
                           mm_X=model.matrix(self$params$formula,data = task$X)
                           fit_object=list(mm_X=mm_X)
                           return(fit_object)

                         },
                         .predict = function(task = NULL) {
                           if(identical(task,private$.training_task)){
                             return(private$.fit_object$mm_X)
                           } else{
                             mm_X=model.matrix(self$params$formula,data = task$X)
                             return(mm_X)
                           }
                         },
                         .chain = function(task = NULL) {
                           predictions = self$predict(task)
                           predictions = as.data.table(predictions)
                           
                           
                           #add predictions as new columns
                           new_col_names = task$add_columns(self$fit_uuid, predictions)
                           
                           return(task$next_in_chain(covariates=new_col_names))
                         }
                       )
)



