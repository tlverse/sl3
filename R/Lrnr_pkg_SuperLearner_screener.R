#support for SuperLearner screeners, obviously not the most efficient approach, we should reimplement as many as possible
#' @importFrom assertthat assert_that is.count is.flag
#' @export
#' @rdname undocumented_learner
Lrnr_pkg_SuperLearner_screener <- R6Class(classname = "Lrnr_pkg_SuperLearner_screener",
                     inherit= Lrnr_base,
                     portable = TRUE,
                     class = TRUE,
                     public = list(
                       initialize = function(SL_wrapper, ...) {
                         if(SL_wrapper == "All"){
                           wrapper_fun = NULL
                         } else {
                           wrapper_fun=get(SL_wrapper)  
                         }
                         
                         params=list(wrapper_name=SL_wrapper, wrapper_fun=wrapper_fun, ...)
                         super$initialize(params=params, ...)
                       }),
                     private = list(
                       .train = function(task) {
                         wrapper=self$params$wrapper_fun
                         if(is.null(wrapper)){
                           selected <- task$nodes$covariates
                         } else {
                           selected <- wrapper(task$Y, task$X, family=gaussian(), obsWeights=task$weights, id = task$id)  
                         }
                         
                         fit_object=list(selected=task$nodes$covariates[selected])
                         
                         return(fit_object)
                         
                       },
                       .predict = function(task) {
                         task$X[,private$.fit_object$selected,with=F, drop=F]
                         
                       },
                       .chain = function(task) {
                         return(task$next_in_chain(covariates=private$.fit_object$selected))
                       },
                       .required_packages = c("SuperLearner")
                       )
                     
)

