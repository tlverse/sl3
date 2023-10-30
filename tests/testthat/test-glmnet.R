context("test-glmnet.R -- Lrnr_glmnet")

library(origami)
library(data.table)

data(cpp_imputed)
covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
task_contY <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")
task_binY <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "sexn")

# Lrnr_glmnet with stratify_cv works
lrnr_glmnet <- Lrnr_glmnet$new()
# expect_warning(ff <- lrnr_glmnet$train(task_contY))
fit <- lrnr_glmnet$train(task_binY)
preds <- fit$predict(task_binY)

test_that("Lrnr_glmnet with offset works", {
  n <- 500
  p <- 4
  W <- matrix(rnorm(n * p), nrow = n)
  colnames(W) <- paste("W", seq_len(p), sep = "")
  g0W <- plogis(0.8 * W[, 1])
  A <- rbinom(n, 1, g0W)
  d <- data.frame(W, A)
  d$offset <- runif(n)
  d_newoffset <- copy(d)
  d_newoffset$offset <- runif(n)

  Wnodes <- grep("^W", names(d), value = TRUE)
  Anode <- "A"
  task <- sl3_Task$new(d, covariates = Wnodes, outcome = Anode)

  offset_task <- sl3_Task$new(
    d,
    covariates = Wnodes, outcome = Anode, offset = "offset"
  )

  newoffset_task <- sl3_Task$new(
    d_newoffset,
    covariates = Wnodes, outcome = Anode, offset = "offset"
  )

  # specifically test lrnr_glm against base glm
  lrnr_glmnet <- make_learner(Lrnr_glmnet, nfolds = 3)

  set.seed(569)
  fit <- lrnr_glmnet$train(task)
  set.seed(569)
  offset_fit <- lrnr_glmnet$train(offset_task)

  preds <- fit$predict()
  offset_preds <- offset_fit$predict()
  expect_false(isTRUE(all.equal(preds, offset_preds)))
  expect_warning(fit$predict(offset_task))
})
