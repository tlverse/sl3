
#' @importFrom R6 R6Class
#' @export
#' @rdname undocumented_learner
Lrnr_glm <- R6Class(classname = "Lrnr_glm", inherit = Lrnr_base, portable = TRUE, 
                    class = TRUE, public = list(
                      initialize = function(family = gaussian(), ...) {
                        params <- list(family = family, ...)
                        super$initialize(params = params, ...)
                      }),
                    private = list(
                      .train = function(task) {
                        params <- self$params
                        family <- params[["family"]]
                        if (is.character(family)) {
                          family <- get(family, mode = "function", envir = parent.frame())
                          family <- family()
                        }
                        # todo: if possible have this use task$Xmat with glm.fit or speedglm
                        Y <- task$Y
                        fit_object <- glm(Y ~ ., data = task$X, family = family, weights = task$weights)
                        
                        return(fit_object)
                        
                      }, 
                      .predict = function(task = NULL) {
                        predictions <- predict(private$.fit_object, newdata = task$X, type = "response")
                        
                        return(predictions)
                        
                      }
                    ), 
)






