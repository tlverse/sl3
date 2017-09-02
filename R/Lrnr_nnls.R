
#' @importFrom R6 R6Class
#' @export
#' @rdname undocumented_learner
Lrnr_nnls <- R6Class(classname = "Lrnr_nnls", inherit = Lrnr_base, portable = TRUE, 
                     class = TRUE, 
                     public = list(
                       initialize = function(...) {
                         params <- list(...)
                         super$initialize(params = params, ...)
                       }), 
                     private = list(
                       .train = function(task) {
                         x <- task$X
                         y <- task$Y
                         fit_object <- nnls::nnls(as.matrix(x), y)
                         
                         return(fit_object)
                         
                       }, 
                       .predict = function(task = NULL) {
                         predictions <- as.matrix(task$X) %*% coef(private$.fit_object)
                         
                         return(predictions)
                         
                       }, 
                       .required_packages = c("nnls")
                     ), 
)






