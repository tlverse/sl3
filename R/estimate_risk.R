#' @export
cv_predict <- function(fold, learner, task) {
    training_task <- training(task)
    validation_task <- validation(task)
    
    fit <- learner$train(training_task)
    predictions <- fit$predict(validation_task)
    cv_preds <- data.table(truth = validation_task$Y, predictions = predictions)
    return(list(cv_preds = cv_preds))
}

#' @export
estimate_risk <- function(learner, task) {
    # todo: this obviously is very minimal
    folds <- make_folds(task$data)
    cv_preds <- cross_validate(cv_predict, folds, learner, task, future.globals = FALSE)$cv_preds
    
    risk <- risk(cv_preds$truth, cv_preds$predictions)
    return(risk)
}
