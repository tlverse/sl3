library(shiny)
library(future)


data(cpp_imputed)
cpp_imputed <- cpp_imputed[sample(nrow(cpp_imputed),10000,replace=T),]
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"


task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome, outcome_type="continuous")

lrnr_glmnet <- Lrnr_glmnet$new()
random_forest <- Lrnr_randomForest$new()
glm_fast <- Lrnr_glm_fast$new()
nnls_lrnr <- Lrnr_nnls$new()

sl <- Lrnr_sl$new(list(random_forest, lrnr_glmnet, glm_fast), nnls_lrnr)

delayed_task <- delayed_learner_train(sl, task)
ncpu <- availableCores()/2
plan(multicore, workers=ncpu)
sched <- Scheduler$new(delayed_task, nworkers=ncpu)
plot_delayed_shiny(sched)