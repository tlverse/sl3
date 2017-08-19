context("Test Model Matrix")

if (FALSE) {
    setwd("..")
    setwd("..")
    getwd()
    library("devtools")
    document()
    load_all("./")  # load all R files in /R and datasets in /data. Ignores NAMESPACE:
    setwd("..")
    install("sl3", build_vignettes = FALSE, dependencies = FALSE)  # INSTALL W/ devtools:
}

library(testthat)
library(sl3)
# library(data.table) library(origami) library(SuperLearner)
set.seed(1)

data(cpp)
cpp <- cpp[!is.na(cpp[, "haz"]), ]
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
cpp[is.na(cpp)] <- 0
outcome <- "haz"

task <- sl3_Task$new(cpp, covariates = covars, outcome = outcome)
task$nodes$covariates

test_that("Lrnr_glm_fast trains based on a subset of covariates (predictors)", {
    fglm_learner <- Lrnr_glm_fast$new(covariates = c("apgar1", "apgar5"))
    
    fGLM_fit <- fglm_learner$train(task)
    # print(fGLM_fit) str(fGLM_fit$params)
    fglm_preds_2 <- fGLM_fit$predict()
    expect_true(data.table::is.data.table(fglm_preds_2))
    
    glm.fit <- glm(haz ~ apgar1 + apgar5, data = cpp, family = stats::gaussian())
    glm_preds_2 <- as.vector(predict(glm.fit))
    
    expect_true(sum(fglm_preds_2 - glm_preds_2) < 10^(-10), )
    expect_true(all.equal(as.vector(glm_preds_2), as.vector(fglm_preds_2[[1]])))
})

test_that("use chaining to subset predictors (Model_Matrix as first learner)", {
    ## suppress intercept here because we're calling model.matrix again in Lrnr_glm
    two_cov <- Lrnr_model.matrix$new(~apgar1 + apgar5 - 1)
    glm_learner <- Lrnr_glm$new()
    glm_subset <- Pipeline$new(two_cov, glm_learner)
    glm_fit <- glm_subset$train(task)
    glm_preds <- glm_fit$predict()
    
    fglm_learner <- Lrnr_glm_fast$new(covariates = c("apgar1", "apgar5"))
    fGLM_fit <- fglm_learner$train(task)
    fglm_learner <- Lrnr_glm_fast$new(covariates = c("apgar1", "apgar5"))
    fglm_preds <- fGLM_fit$predict()
    
    expect_true(all.equal(as.vector(glm_preds), as.vector(fglm_preds[[1]])))
})

test_that("model matrix defines interactions", {
    # suppress intercept here because we're calling model.matrix again in Lrnr_glm
    interactions <- Lrnr_model.matrix$new(~apgar1 * apgar5 - 1)
    glm_learner <- Lrnr_glm$new()
    glm_interactions <- Pipeline$new(interactions, glm_learner)
    glm_fit <- glm_interactions$train(task)
    glm_preds <- glm_fit$predict()
    fglm_learner <- Lrnr_glm_fast$new(covariates = c("apgar1", "apgar5"), interactions = list(c("apgar1", 
        "apgar5")))
    fGLM_fit <- fglm_learner$train(task)
    fglm_preds <- fGLM_fit$predict()
    expect_true(all.equal(as.vector(glm_preds), as.vector(fglm_preds[[1]])))
})
