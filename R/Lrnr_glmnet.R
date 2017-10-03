#support for SuperLearner wrappers, obviously not the most efficient approach, we should reimplement as many as possible
#' @importFrom assertthat assert_that is.count is.flag
#' @export
#' @rdname undocumented_learner
Lrnr_glmnet <- R6Class(classname = "Lrnr_glmnet",
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
                                     X <- as.matrix(task$X)
                                     Y <- task$Y
                                     weights <- task$weights
                                     family <- params$family
                                     fit_object <- glmnet::cv.glmnet(x = X, y = Y, weights = weights, 
                                                                      lambda = NULL, type.measure = "deviance", nfolds = 10, 
                                                                      family = family, alpha = 1, nlambda = 100)
                                     return(fit_object)
                                     
                                   },
                                   .predict = function(task){
                                     predictions = predict(private$.fit_object, newdata=task$X)
                                     return(predictions)
                                   },
                                   .required_packages = c("randomForest")
                                   
                                 )
                                 
)

