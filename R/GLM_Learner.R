# not only prediction, but also basis functions, variable selection, data cleaning, etc I think this object can
# also store the trains somehow the predict method needs to output something conducive to chaining (i.e. a new
# set of covariates or outcome or something) I guess that's a new learner task specifies covariates and outcome
# and outcome type
#' @importFrom assertthat assert_that is.count is.flag
#' @export
GLM_Learner <- R6Class(classname = "GLM_Learner", inherit = Learner, portable = TRUE, class = TRUE, private = list(.train = function(task) {
    # todo: if possible have this use task$Xmat with glm.fit or speedglm
    Y <- task$Y
    fit_object <- glm(Y ~ ., data = task$X, family = gaussian(), weights = task$weights)
    
    return(fit_object)
    
}, .predict = function(task = NULL) {
    predictions <- predict(private$.fit_object, newdata = task$X, type = "response")
    
    return(predictions)
    
}), )






