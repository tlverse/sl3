#support for SuperLearner wrappers, obviously not the most efficient approach, we should reimplement as many as possible
#' @importFrom assertthat assert_that is.count is.flag
#' @export
#' @rdname undocumented_learner
Lrnr_randomForest <- R6Class(classname = "Lrnr_randomForest",
                                 inherit= Lrnr_base,
                                 portable = TRUE,
                                 class = TRUE,
                                 public = list(
                                   initialize = function(...) {
                                     
                                     params=list(...)
                                     super$initialize(params=params, ...)
                                   }),
                                 private = list(
                                   
                                   .train = function(task) {
                                     X <- task$X
                                     Y <- task$Y
                                     fit_object <- randomForest::randomForest(y = Y, 
                                                                          x = X, ntree = 100, keep.forest = TRUE, 
                                                                          mtry = floor(ncol(X)), nodesize = 5, maxnodes = NULL, 
                                                                          importance = FALSE)
                                     return(fit_object)
                                     
                                   },
                                   .predict = function(task){
                                     predictions = predict(private$.fit_object, newdata=task$X)
                                     return(predictions)
                                   },
                                   .required_packages = c("randomForest")
                                   
                                 )
                                 
)

