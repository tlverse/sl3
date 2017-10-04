#' @importFrom R6 R6Class
#' @export
#' @rdname undocumented_learner
Lrnr_mean <- R6Class(classname = "Lrnr_mean", inherit = Lrnr_base, portable = TRUE,
                     class = TRUE,
                     public = list(
                       initialize = function(...) {
                         params <- list(...)
                         super$initialize(params = params, ...)
                       },
                       print = function(){
                         print(self$name)
                       }),
                     private = list(
                       .train = function(task) {
                         y <- task$Y
                         fit_object <- list(mean=mean(y))
                         return(fit_object)
                       },
                       .predict = function(task = NULL) {
                         predictions <- rep(private$.fit_object$mean, task$nrow)
                         return(predictions)
                       }
                     ),
)






