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
task <- sl3_Task$new(mtcars, covariates = covariates, outcome = outcome)

lrnr_glm <- make_learner(Lrnr_glm_fast)
lrnr_mean <- make_learner(Lrnr_mean)
lrnr_sl_subset <- make_learner(Lrnr_sl, c(lrnr_glm, lrnr_mean), covariates=c("cyl","disp","hp"))
fit <- lrnr_sl_subset$train(task)
pred <- fit$predict(task)
