library(data.table)
library(sl3)

data(cpp_imputed)
covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")

task <- sl3_Task$new(cpp_imputed[1:1000,], covariates = covs, outcome = "haz")
task_pred <- sl3_Task$new(cpp_imputed[1001:1441,], covariates = covs, outcome = "haz")

Lrnr_quantile <- Lrnr_quantile_dist_glmnet$new(lambda = NULL, quantiles = 20, degree = 2, q_tails = 0.1)
fit <- Lrnr_quantile$train(task)
preds <- fit$predict(task_pred)


Lrnr_quantile_dist_glmnet_1 <- Lrnr_quantile_dist_glmnet$new(lambda = NULL, quantiles = 4, degree = 2, q_tails = 0.05)
Lrnr_quantile_dist_glmnet_2 <- Lrnr_quantile_dist_glmnet$new(lambda = NULL, quantiles = 10, degree = 2, q_tails = 0.1)
Lrnr_quantile_dist_glmnet_3 <- Lrnr_quantile_dist_glmnet$new(lambda = NULL, quantiles = 30, degree = 2, q_tails = 0.1)

fit <- Lrnr_quantile_dist_glmnet_3$train(task)
preds <- fit$predict(task)

learners <- c(Lrnr_quantile_dist_glmnet_1, Lrnr_quantile_dist_glmnet_2, Lrnr_quantile_dist_glmnet_3)
Exposures_stack <- make_learner(Stack, learners)

# task <- sl3::make_sl3_Task(
#   data = data, covariates = c("M1", "M2", "M3"),
#   outcome = "Y",
#   outcome_type = "continuous"
# )

discrete_sl_metalrn <- sl3::Lrnr_cv_selector$new()

discrete_sl <- sl3::Lrnr_sl$new(
  learners = Exposures_stack,
  metalearner = discrete_sl_metalrn)

sl_fit <- discrete_sl$train(task)


########### continuous test 

data_info <- simulate_data()
data <- data_info$data

cont_task <- sl3_Task$new(data[1:1500,], covariates = c("M1", "M2", "M3"), outcome = "Y")
cont_task_pred <- sl3_Task$new(data[1501:2000,], covariates = c("M1", "M2", "M3"), outcome = "Y")

Lrnr_quantile <- Lrnr_quantile_dist_glmnet$new(lambda = NULL, quantiles = 20, degree = 2, q_tails = 0.1)
fit <- Lrnr_quantile$train(cont_task)
preds <- fit$predict(cont_task_pred)




