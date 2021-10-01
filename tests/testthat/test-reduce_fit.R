
set.seed(1234)

# TODO: maybe check storage at different n to get rate
n <- 1e3
p <- 100
# these two define the DGP (randomly)
p_X <- runif(p, 0.2, 0.8)
beta <- rnorm(p)

# simulate from the DGP
X <- sapply(p_X, function(p_Xi) rbinom(n, 1, p_Xi))
p_Yx <- plogis(X %*% beta)
Y <- rbinom(n, 1, p_Yx)
data <- data.table(X, Y)

# generate the sl3 task and learner
outcome <- "Y"
covariates <- setdiff(names(data), outcome)
task <- make_sl3_Task(data, covariates, outcome)

options(sl3.verbose = TRUE)
options(sl3.reduce_fit = TRUE)
test_reduce_fit <- function(learner) {
  fit <- learner$train(task)
  print(sl3:::check_fit_sizes(fit))
  if (!getOption("sl3.reduce_fit")) {
    # if we aren't automatically reducing, do it manually
    fit_object <- fit$reduce_fit()
  }

  still_present <- intersect(
    names(fit$fit_object),
    fit$.__enclos_env__$private$.fit_can_remove
  )

  expect_equal(length(still_present), 0)
}

test_reduce_fit(make_learner(Lrnr_glmnet))
test_reduce_fit(make_learner(Lrnr_ranger))
test_reduce_fit(make_learner(Lrnr_glm_fast))
test_reduce_fit(make_learner(Lrnr_xgboost))
test_reduce_fit(make_learner(Lrnr_hal9001))
