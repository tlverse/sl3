library(partykit)
context("test-Lrnr_glmtree.R -- General testing for GlMtree")

# define test dataset
data(mtcars)
task <- sl3_Task$new(mtcars, covariates = c(
  "cyl", "disp", "hp", "drat", "wt", "qsec",
  "vs", "am", "gear", "carb"
), outcome = "mpg")

test_that("Lrnr_glmtree continuous outcome preds match those from glmtree", {
  ## instantiate Lrnr_glmtree, train on task, and predict on task
  lrnr_glmtree <- Lrnr_glmtree$new()
  fit_lrnr_glmtree <- lrnr_glmtree$train(task)
  prd_lrnr_glmtree <- fit_lrnr_glmtree$predict()

  ## fit glmtree using the data from the task
  fit_glmtree <- glmtree(mpg ~ ., data = task$data)
  prd_glmtree <- predict(fit_glmtree, newdata = task$data)

  ## test equivalence of prediction from Lrnr_glmtree and glmtree::glmtree
  expect_equal(prd_lrnr_glmtree, prd_glmtree)
})

test_that("Lrnr_glmtree includes offset correctly", {
  
  task <- sl3_Task$new(mtcars, 
                       covariates = c("disp", "hp",  "wt"), 
                       outcome = "mpg",
                       offset = "drat")
  
  
  ## instantiate Lrnr_glmtree, train on task, and predict on task
  lrnr_glmtree_1 <- Lrnr_glmtree$new(alpha = 0.9)
  lrnr_glmtree_2 <- Lrnr_glmtree$new(alpha = 0.9, prune = "AIC")
  lrnr_glmtree_3 <- Lrnr_glmtree$new(alpha = 0.9, prune = "AIC", maxdepth = 4)
  
  learners <- c( lrnr_glmtree_1, lrnr_glmtree_2, lrnr_glmtree_3)
  discrete_sl_metalrn <- Lrnr_cv_selector$new()
  
  tree_stack <- make_learner(Stack, learners)
  
  discrete_tree_sl <- Lrnr_sl$new(
    learners = tree_stack,
    metalearner = discrete_sl_metalrn
  )
  
  fit_lrnr_glmtree <- discrete_tree_sl$train(task)
  prd_lrnr_glmtree <- fit_lrnr_glmtree$predict()
  
  formula <- as.formula(mpg ~ disp + hp + wt)
  drat <- task$data$drat
  ## fit glmtree using the data from the task
  fit_glmtree <- glmtree(formula, offset = drat, data = task$data)
  prd_glmtree <- predict(fit_glmtree, newdata = task$data)
  
  ## test equivalence of prediction from Lrnr_glmtree and glmtree::glmtree
  expect_equal(prd_lrnr_glmtree, as.vector(prd_glmtree), tolerance = 0.01)
})
