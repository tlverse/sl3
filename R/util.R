true_obj_size <- function(obj) {
    length(serialize(obj, NULL))
}
reduce_fit_test <- function(learner_fit) {
    # given a learner fit, sequentially drop components from the internal fit object, keeping track of which
    # components are needed for prediction
    
    # learner_fit = glm_fit
    original_fit <- learner_fit$fit_object
    task <- learner_fit$training_task
    
    original_size <- true_obj_size(original_fit)
    original_predict <- learner_fit$predict()
    
    components <- names(original_fit)
    reduced <- learner_fit$clone()
    reduced_fit <- original_fit
    for (component in components) {
        backup <- reduced_fit[component]
        reduced_fit[component] <- NULL
        reduced$set_train(reduced_fit, task)
        reduced_predict <- NULL
        try({
            reduced_predict <- reduced$predict()
        }, silent = TRUE)
        if (!identical(original_predict, reduced_predict)) {
            reduced_fit[component] <- backup
        }
    }
    
    reduced_components <- names(reduced_fit)
    
    reduced_size <- true_obj_size(reduced_fit)
    reduction <- as.numeric(1 - reduced_size/original_size)
    try()
    
    
}
