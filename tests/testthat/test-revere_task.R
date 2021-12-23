context("test-Lrnr_revere_task.R -- Lrnr that generates revere task")


data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"
task <- sl3_Task$new(data.table::copy(cpp_imputed), covariates = covars, outcome = outcome)

lrnr_glm <- make_learner(Lrnr_glm)
lrnr_mean <- make_learner(Lrnr_mean)
stack <- make_learner(Stack, lrnr_glm, lrnr_mean)
metalearner <- make_learner(Lrnr_nnls)
sl <- Lrnr_sl$new(learners = stack, metalearner = metalearner)

fit <- sl$train(task)
fold_number <- "validation"
# subset_revere_fun_generator <- function(fit, threshold, keep_vars)
revere_subset_vim_fit <- function(task, fold_number) {
  if (fold_number == "validation") {
    return(task)
  }
  vim <- importance(fit, loss_squared_error, fold_number)
  keep_vars <- unlist(vim[1:3, "covariate"], use.names = FALSE)
  subset_task <- task$next_in_chain(covariates = keep_vars)

  return(subset_task)
}

lrnr_subset_revere <- make_learner(Lrnr_revere_task, revere_subset_vim_fit)

lsr_fit <- lrnr_subset_revere$train(task)
chained_revere_task <- lsr_fit$chain()
subset_test <- chained_revere_task$revere_fold_task("full")
pipe <- make_learner(Pipeline, lrnr_subset_revere, sl)
pipe_fit <- pipe$train(task)
pipe_preds <- pipe_fit$predict()
pipe_val_preds <- pipe_fit$predict_fold(task, "validation")
