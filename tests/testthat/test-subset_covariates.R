context("test-sl3_task -- Basic sl3_Task functionality")
library(data.table)
library(uuid)


# define test dataset
data(mtcars)
covariates <- c(
  "cyl", "disp", "hp", "drat", "wt", "qsec",
  "vs", "am", "gear", "carb"
)
outcome <- "mpg"

covariate_subset <- c("cyl", "disp", "hp")
task <- sl3_Task$new(mtcars, covariates = covariates, outcome = outcome)

lrnr_glm <- make_learner(Lrnr_glm_fast)
lrnr_mean <- make_learner(Lrnr_mean)
lrnr_sl_subset <- make_learner(Lrnr_sl, c(lrnr_glm, lrnr_mean), covariates = covariate_subset)
fit <- lrnr_sl_subset$train(task)
pred <- fit$predict(task)

glm_full_fit <- fit$fit_object$full_fit$fit_object$learner_fits[[1]]$fit_object$learner_fits[[1]]
glm_coef_names <- names(coef(glm_full_fit))
test_that("subsetting covariates extends to sublearners", expect_equal(glm_coef_names, c("intercept", covariate_subset)))

task_pre_subset <- sl3_Task$new(mtcars, covariates = covariate_subset, outcome = outcome)

glm_fit_pre_subset <- lrnr_glm$train(task_pre_subset)
full_preds <- glm_fit_pre_subset$predict(task)
training_preds <- glm_fit_pre_subset$predict()
test_that("extra covariates in prediction set get dropped correctly", expect_equal(full_preds, training_preds))
