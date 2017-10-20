
data(cpp)
cpp <- cpp[!is.na(cpp[, "haz"]), ]
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
cpp[is.na(cpp)] <- 0
cpp <- cpp[sample(nrow(cpp),1e6,replace=T),]
# cpp <- cpp[1:150, ]
outcome <- "haz"
task <- sl3_Task$new(cpp, covariates = covars, outcome = outcome, outcome_type="continuous")

glm_fast <- make_learner(Lrnr_glm_fast)

training_time <- system.time({fit <- glm_fast$train(task)})
pred_time <- system.time({preds <- fit$predict(task)})
fold_time <- system.time({task$folds})