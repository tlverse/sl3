library(partykit)
context("test-Lrnr_glmtree.R -- General testing for GLMtree")

# define test dataset
data(mtcars)

test_that("Lrnr_glmtree continuous outcome preds match those from glmtree", {
  task <- sl3_Task$new(mtcars, covariates = c(
    "cyl", "disp", "hp", "drat", "wt", "qsec",
    "vs", "am", "gear", "carb"
  ), outcome = "mpg")

  ## instantiate Lrnr_glmtree, train on task, and predict on task
  lrnr_glmtree <- Lrnr_glmtree$new()
  fit_lrnr_glmtree <- lrnr_glmtree$train(task)
  prd_lrnr_glmtree <- fit_lrnr_glmtree$predict()

  ## fit glmtree using the data from the task
  fit_glmtree <- glmtree(mpg ~ ., data = task$data)
  prd_glmtree <- predict(fit_glmtree, newdata = task$data)

  ## test equivalence of prediction from Lrnr_glmtree and glmtree::glmtree
  expect_equal(prd_lrnr_glmtree, as.numeric(prd_glmtree))
})

test_that("Lrnr_glmtree binary outcome preds match those from glmtree", {
  task <- sl3_Task$new(mtcars, covariates = c(
    "cyl", "disp", "hp", "drat", "wt", "qsec",
    "mpg", "am", "gear", "carb"
  ), outcome = "vs")

  ## instantiate Lrnr_glmtree, train on task, and predict on task
  lrnr_glmtree <- Lrnr_glmtree$new()
  fit_lrnr_glmtree <- lrnr_glmtree$train(task)
  prd_lrnr_glmtree <- fit_lrnr_glmtree$predict()

  ## fit glmtree using the data from the task
  fit_glmtree <- glmtree(vs ~ ., data = task$data)
  prd_glmtree <- predict(fit_glmtree, newdata = task$data)

  ## test equivalence of prediction from Lrnr_glmtree and glmtree::glmtree
  expect_equal(prd_lrnr_glmtree, as.numeric(prd_glmtree))
})

test_that("Lrnr_glmtree includes offset correctly", {
  task <- sl3_Task$new(mtcars,
    covariates = c("disp", "hp", "wt"),
    outcome = "mpg",
    offset = "drat"
  )

  ## instantiate Lrnr_glmtree, train on task, and predict on task
  lrnr_glmtree <- Lrnr_glmtree$new(alpha = 0.9, prune = "AIC")
  fit_lrnr_glmtree <- lrnr_glmtree$train(task)
  prd_lrnr_glmtree <- suppressWarnings(fit_lrnr_glmtree$predict())

  # fit glmtree with same specification as sl3
  # we need to set max_depth=10, since that was set as the default in
  # Lrnr_glmtree and it is different from the partykit::glmtree default of Inf
  d <- task$data
  fit_glmtree <- do.call(partykit::glmtree, list(
    formula = as.formula(mpg ~ offset(drat) | disp + hp + wt),  data = d,
    alpha = 0.9, prune = "AIC"
  ))
  prd_glmtree <- suppressWarnings(predict(fit_glmtree, newdata = d))

  ## test equivalence of prediction from Lrnr_glmtree and partykit::glmtree
  expect_equal(prd_lrnr_glmtree, as.numeric(prd_glmtree))
})

test_that("Lrnr_glmtree errors when covariates in formula are misspecified", {
  task <- sl3_Task$new(mtcars,
    covariates = c("disp", "hp", "wt"),
    outcome = "mpg",
    offset = "drat"
  )

  ## instantiate Lrnr_glmtree, train on task, and predict on task
  lrnr_glmtree <- Lrnr_glmtree$new(formula = "mpg ~ drat + disp")
  expect_error(lrnr_glmtree$train(task))
})

test_that("Lrnr_glmtree errors when outcome in formula is misspecified", {
  task <- sl3_Task$new(mtcars,
    covariates = c("disp", "hp", "wt"),
    outcome = "mpg",
    offset = "drat"
  )

  ## instantiate Lrnr_glmtree, train on task, and predict on task
  lrnr_glmtree <- Lrnr_glmtree$new(formula = "hp ~ wt + disp")
  expect_error(lrnr_glmtree$train(task))
})
