#support for converting data frame with raw columns into a model matrix with things like interaction terms and factor indicators. todo: reimplement this without using model.matrix.
#' @importFrom assertthat assert_that is.count is.flag
#' @export
Model_Matrix <- R6Class(classname = "Model_Matrix",
                       inherit= Learner,
                       portable = TRUE,
                       class = TRUE,
                       public = list(
                         initialize = function(formula) {
                           params=list(formula=formula)
                           super$initialize(params=params)
                         }),
                       private = list(
                         .train = function(task) {
                           #todo: think about whether to do this here, or only on calling predict
                           mm_X=model.matrix(self$params$formula,data = task$X)
                           mm_task=task$next_in_chain(mm_X)
                           fit_object=list(mm_task=mm_task)
                           
                           return(fit_object)
                           
                         },
                         .predict = function(task = NULL) {
                           if(identical(task,private$.training_task)){
                             return(private$.fit_object$mm_task$X)
                           } else{
                             mm_X=model.matrix(self$params$formula,data = task$X)
                             return(mm_X)
                           }
                         },
                         .chain = function(task = NULL) {
                           if(identical(task,private$.training_task)){
                             return(private$.fit_object$mm_task)
                           } else{
                             mm_X=model.matrix(self$params$formula,data = task$X)
                             mm_task=task$next_in_chain(mm_X)
                             return(mm_task)
                           }
                         }
                         
                       )
                       
)


  
