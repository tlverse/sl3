context("test-pca.R -- Lrnr_pca with and without Pipelines")
library(origami)
library(dplyr)

# data
data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
            "sexn")
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covars, outcome = outcome)

# define learners
glm_learner <- Lrnr_glm$new(intercept = FALSE)
pca_learner <- Lrnr_pca$new()
pca_and_glm <- Pipeline$new(pca_learner, glm_learner)
stack <- Stack$new(pca_and_glm, glm_learner)
stack_fit <- stack$train(task)
stack_fit_out <- stack_fit$predict()

# compute PCA with GLM manually
ncomp <- 2
cpp_pca <- cpp_imputed %>%
  dplyr::select(covars) %>%
  prcomp(x = ., center = TRUE, scale = TRUE)
cpp_pca_rotated <- cpp_pca$x[, seq_len(ncomp)]
pcr_cpp <- glm(cpp_imputed$haz ~ -1 + cpp_pca_rotated)
pcr_preds <- predict(pcr_cpp) %>%
  as.numeric()

