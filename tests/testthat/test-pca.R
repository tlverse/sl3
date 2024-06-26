context("test-pca.R -- Lrnr_pca for preprocessing with Pipelines")
library(origami)
library(dplyr)
set.seed(37912)

# data
ncomp <- 3
data(cpp_imputed)
covars <- c(
  "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
  "sexn"
)
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

# define learners
glm_fast <- Lrnr_glm$new(intercept = FALSE)
pca_sl3 <- Lrnr_pca$new(n_comp = ncomp, center = TRUE, scale. = TRUE)
pcr_pipe_sl3 <- Pipeline$new(pca_sl3, glm_fast)

# create stacks + train and predict
pcr_pipe_sl3_fit <- pcr_pipe_sl3$train(task)
out_pcr_fit <- pcr_pipe_sl3_fit$predict()

# extract prcomp object (PCA results) from Pipeline
pca_from_pipe <- pcr_pipe_sl3_fit$fit_object$learner_fits[[1]]$fit_object

# compute PCA with GLM manually
cpp_pca <- cpp_imputed %>%
  dplyr::select(all_of(covars)) %>%
  stats::prcomp(x = ., center = TRUE, scale. = TRUE)
cpp_pca_rotated <- cpp_pca$x[, seq_len(ncomp)]
pcr_cpp <- glm(cpp_imputed$haz ~ -1 + cpp_pca_rotated)
pcr_preds <- predict(pcr_cpp) %>% as.numeric()


test_that("PCA computed by Lrnr_pca matches stats::prcomp exactly", {
  dimnames(cpp_pca$x) <- list(NULL, dimnames(cpp_pca$x)[[2]])
  expect_equal(pca_from_pipe, cpp_pca)
})

test_that("Regression on PCs matches between Pipeline and manual run", {
  expect_identical(out_pcr_fit, pcr_preds)
})

test_that("Arguments are passed to prcomp correctly by Lrnr_pca", {
  # pass some arguments to prcomp via Lrnr_pca
  pca_sl3 <- Lrnr_pca$new(
    n_comp = ncomp, retx = FALSE, center = TRUE,
    scale. = FALSE
  )
  pcr_pipe_sl3 <- Pipeline$new(pca_sl3, glm_fast)
  pcr_pipe_sl3_fit <- pcr_pipe_sl3$train(task)
  pca_from_pipe <- pcr_pipe_sl3_fit$fit_object$learner_fits[[1]]$fit_object

  # do the same thing with prcomp
  cpp_pca <- cpp_imputed %>%
    dplyr::select(covars) %>%
    stats::prcomp(x = ., retx = FALSE, center = TRUE, scale. = FALSE)

  expect_equal(pca_from_pipe, cpp_pca)
})
