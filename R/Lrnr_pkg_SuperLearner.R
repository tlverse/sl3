#support for SuperLearner wrappers, obviously not the most efficient approach, we should reimplement as many as possible
#' @importFrom assertthat assert_that is.count is.flag
#' @export
#' @rdname undocumented_learner
Lrnr_pkg_SuperLearner <- R6Class(classname = "Lrnr_pkg_SuperLearner",
                     inherit= Lrnr_base,
                     portable = TRUE,
                     class = TRUE,
                     public = list(
                       initialize = function(SL_wrapper, ...) {
                         wrapper_fun=get(SL_wrapper)
                         params=list(wrapper_name=SL_wrapper, wrapper_fun=wrapper_fun, ...)
                         super$initialize(params=params, ...)
                       }),
                     private = list(
                       .train = function(task) {
                         wrapper=self$params$wrapper_fun

                         #to minimize prediction costs (since we throw out preds from here anyways), newX is just a single row
                         newX=task$X[1,]

                         fit_object <- wrapper(task$Y, task$X, newX, family=gaussian(), obsWeights=task$weights, id= task$id)$fit

                         return(fit_object)

                       },
                       .required_packages = c("SuperLearner")
                       # generic learner predict suffices
                       )

)

