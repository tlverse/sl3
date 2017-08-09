
#' @importFrom R6 R6Class
#' @rdname undocumented_learner
GLM_Learner <- R6Class(classname = "GLM_Learner", inherit = Learner, portable = TRUE, class = TRUE, private = list(.train = function(task) {
    # todo: if possible have this use task$Xmat with glm.fit or speedglm
    Y <- task$Y
    fit_object <- glm(Y ~ ., data = task$X, family = gaussian(), weights = task$weights)
    
    return(fit_object)
    
}, .predict = function(task = NULL) {
    predictions <- predict(private$.fit_object, newdata = task$X, type = "response")
    
    return(predictions)
    
}), )






