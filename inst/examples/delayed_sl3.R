library(sl3)
library(shiny)
library(future)


data(cpp_imputed)
cpp_imputed <- cpp_imputed[sample(nrow(cpp_imputed), 10000, replace = T), ]
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"

options(sl3.save.training = TRUE)
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome, outcome_type = "continuous")

lrnr_glmnet <- Lrnr_glmnet$new()
random_forest <- Lrnr_randomForest$new()
glm_fast <- Lrnr_glm_fast$new()
nnls_lrnr <- Lrnr_nnls$new()

sl <- Lrnr_sl$new(list(random_forest, lrnr_glmnet, glm_fast), nnls_lrnr)

delayed_task <- delayed_learner_train(sl, task)
delayed_task$compute()

rm(delayed_task)
serial_size <- function(x) {
  length(serialize(x, NULL)) / 2^20
}
serial_size(delayed_task)
fit <- delayed_task$value
serial_size(fit)

ncpu <- availableCores() / 2
plan(multicore, workers = ncpu)
ncpu <- 1
sched <- Scheduler$new(delayed_task, nworkers = ncpu)
sched$compute_step()
plot_delayed_shiny(sched)
delayed_task$compute()
