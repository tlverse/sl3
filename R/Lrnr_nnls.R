#' @importFrom R6 R6Class
#' @export
#' @rdname undocumented_learner
Lrnr_nnls <- R6Class(classname = "Lrnr_nnls", inherit = Lrnr_base, portable = TRUE,
                     class = TRUE,
                     public = list(
                       initialize = function(...) {
                         params <- list(...)
                         super$initialize(params = params, ...)
                       },
                       print = function(){
                         print(self$name)
                         print(self$fits)
                       }),
                     active = list(
                       fits = function() {
                         fit_object = private$.fit_object
                         if (!is.null(fit_object)) {
                           data.table(lrnrs = fit_object$lrnrs, weights = fit_object$x)
                         } else {
                           data.table(lrnrs = character(0), weights = numeric(0))
                         }
                       }
                     ),
                     private = list(
                       .train = function(task) {
                         x <- task$X
                         y <- task$Y
                         fit_object <- nnls::nnls(as.matrix(x), y)
                         fit_object$lrnrs <- names(task$X)
                         return(fit_object)
                       },
                       .predict = function(task = NULL) {
                         predictions <- as.matrix(task$X) %*% coef(private$.fit_object)
                         return(predictions)
                       },
                       .required_packages = c("nnls")
                     ),
)






