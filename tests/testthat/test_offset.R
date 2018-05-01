context("test_offset.R -- offset handling")
library(origami)


g0 <- function(W) {
  W1 <- W[, 1]
  scale_factor <- 0.8
  A <- plogis(scale_factor * W1)
}

gen_data <- function(n = 1000, p = 4) {
  W <- matrix(rnorm(n * p), nrow = n)
  colnames(W) <- paste("W", seq_len(p), sep = "")
  g0W <- g0(W)
  A <- rbinom(n, 1, g0W)

  u <- runif(n)
  df <- data.frame(W, A)

  df$g0W <- g0(W)

  return(df)
}

set.seed(1234)
n <- 1000
data <- gen_data(n)
data$offset <- runif(n)
data_newoffset <- copy(data)
data_newoffset$offset <- runif(n)

Wnodes <- grep("^W", names(data), value = TRUE)
Anode <- "A"
task <- sl3_Task$new(data, covariates = Wnodes, outcome = Anode)

offset_task <- sl3_Task$new(
  data,
  covariates = Wnodes, outcome = Anode,
  offset = "offset"
)

newoffset_task <- sl3_Task$new(
  data_newoffset,
  covariates = Wnodes,
  outcome = Anode, offset = "offset"
)

# specifically test lrnr_glm against base glm
lrnr_glm <- make_learner(Lrnr_glm_fast)

fit <- lrnr_glm$train(task)
offset_fit <- lrnr_glm$train(offset_task)

preds <- fit$predict()
offset_preds <- offset_fit$predict()
expect_false(isTRUE(all.equal(preds, offset_preds)))

glm_fit <- glm(A ~ W1 + W2 + W3 + W4, data, family = binomial())
expect_equivalent(coef(glm_fit), coef(fit))

glm_offset_fit <- glm(
  A ~ W1 + W2 + W3 + W4, data,
  family = binomial(),
  offset = qlogis(data$offset)
)
expect_equivalent(coef(glm_offset_fit), coef(offset_fit))

# test generally that offsets work for learners that should support them
test_learner_offset_support <- function(learner,
                                        task,
                                        offset_task,
                                        newoffset_task) {
  cat(sprintf("Verifying offsets for %s\n", learner$name))

  fit <- learner$train(task)
  offset_fit <- learner$train(offset_task)

  # verify that offset changes preds
  preds <- fit$predict()
  offset_preds <- offset_fit$predict()
  expect_false(isTRUE(all.equal(preds, offset_preds)))

  ## TODO: offset verification: No offset -- throw error
  ## break if trained with offset but predicting on new data with no offset
  expect_error(offset_fit$predict(task))

  ## TODO: offset verification: Different offset -- use new offset
  ## work if predicting on task with different offset and trained with offset
  ## should create predictions from new offsets, different from normal $predict
  newoffset_preds <- offset_fit$predict(newoffset_task)
  expect_false(isTRUE(all(offset_preds == newoffset_preds)))

  ## TODO: offset verification: something about offset transformations
  ## sl3 options: transform offsets
  ## this is tricky -- e.g., Lrnr_glm_fast doesn't even store offsets...
}

# instantiate these manually for now, because some have required args
# offset_learners <- c("Lrnr_glm", "Lrnr_glm_fast", "Lrnr_h2o_glm",
# "Lrnr_h2o_grid", "Lrnr_optim", "Lrnr_solnp",
# "Lrnr_xgboost")
offset_learner_stack <- make_learner_stack(
  "Lrnr_glm", "Lrnr_glm_fast",
  "Lrnr_mean", "Lrnr_xgboost"
)

offset_learners <- offset_learner_stack$params$learners
lapply(
  offset_learners, test_learner_offset_support,
  task = task,
  offset_task = offset_task, newoffset_task = newoffset_task
)

# TODO: check that offsets don't apply to Lrnr_sl metalearners (by default)
