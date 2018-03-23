context("test-pca.R -- Lrnr_pca for preprocessing with Pipelines")
library(origami)
library(dplyr)
library(h2o)
h2o.init()

# data
data(cpp_imputed)
covars <- c(
  "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
  "sexn"
)
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

# define learners
fglm_learner <- Lrnr_glm_fast$new(intercept = FALSE)
pca_learner <- Lrnr_pca$new(n_comp = 3, center = TRUE, scale. = TRUE)
pca_h2o <- Lrnr_h2o_mutator$new(
  algorithm = "pca", k = 3,
  impute_missing = TRUE
)
pca_to_glm <- Pipeline$new(pca_learner, fglm_learner)
pca_to_glm_h2o <- Pipeline$new(pca_h2o, fglm_learner)

# create stacks + train and predict
stack_mods <- Stack$new(pca_to_glm, pca_to_glm_h2o, fglm_learner)
fit_stack <- stack_mods$train(task)
out_stack <- fit_stack$predict()

# compute PCA with GLM manually
ncomp <- 3
cpp_pca <- cpp_imputed %>%
  dplyr::select(covars) %>%
  prcomp(x = ., center = TRUE, scale. = FALSE)
cpp_pca_rotated <- cpp_pca$x[, seq_len(ncomp)]
pcr_cpp <- glm(cpp_imputed$haz ~ -1 + cpp_pca_rotated)
pcr_preds <- predict(pcr_cpp) %>%
  as.numeric()

# check whether these values match cpp_pca -- they should!
pca_from_stack <- fit_stack$fit_object$learner_fits[[1]]$fit_object$learner_fits[[1]]
