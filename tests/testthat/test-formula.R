context("test-formula.R -- Handling of learner param formula")

options(java.parameters = "-Xmx2500m")
data(cpp_imputed)
covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"
task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = outcome)

test_learner_with_formula <- function(lrnr, task, ...) {
  print(sprintf("Testing formula with learner: %s", lrnr))

  test_that(paste0(lrnr, " with formula trained to task respecting it"), {
    learner_obj <- make_learner(
      lrnr,
      formula = as.formula("haz ~ apgar1:apgar5 + I(apgar1^2)"), ...
    )
    fit <- learner_obj$train(task)
    formula_cols <- c("I(apgar1^2)", "apgar1:apgar5")
    expect_true(
      all(formula_cols %in% fit$training_task$internal_data$column_names)
    )
  })

  test_that(paste0(lrnr, " with . in formula trained to task respecting it"), {
    learner_obj <- make_learner(
      lrnr,
      formula = as.formula("~.^2"), ...
    )
    fit <- learner_obj$train(task)
    formula_cols <- c(
      "apgar1:apgar5", "apgar1:parity", "apgar1:gagebrth", "apgar1:mage",
      "apgar1:meducyrs", "apgar1:sexn", "apgar5:parity", "apgar5:gagebrth",
      "apgar5:mage", "apgar5:meducyrs", "apgar5:sexn", "parity:gagebrth",
      "parity:mage", "parity:meducyrs", "parity:sexn", "gagebrth:mage",
      "gagebrth:meducyrs", "gagebrth:sexn", "mage:meducyrs", "mage:sexn",
      "meducyrs:sexn"
    )
    expect_true(
      all(formula_cols %in% fit$training_task$internal_data$column_names)
    )
  })

  test_that(paste0(lrnr, " with formula error if regressors not in task"), {
    learner_obj <- make_learner(
      lrnr,
      formula = as.formula("haz ~ X"), ...
    )
    expect_error(fit <- learner_obj$train(task))
  })

  test_that(paste0(lrnr, " with formula error if response not in task"), {
    learner_obj <- make_learner(
      lrnr,
      formula = as.formula("Y ~ apgar1:apgar5"), ...
    )
    expect_error(fit <- learner_obj$train(task))
  })
}

# get learners
cont_learners <- sl3::sl3_list_learners("continuous")
bin_learners <- sl3::sl3_list_learners("binomial")
# bin_learners[-which(bin_learners %in% cont_learners)] 0
ts <- sl3::sl3_list_learners("timeseries")
screen <- sl3::sl3_list_learners("screener")
wrap <- sl3::sl3_list_learners("wrapper")
h2o <- sl3::sl3_list_learners("h2o")
learners <- cont_learners[-which(cont_learners %in% c(ts, screen, wrap, h2o))]

# remove learners that bypass formula param (i.e., they overwrite process_formula)
learners <- learners[!(learners == "Lrnr_glm_semiparametric")]
learners <- learners[!(learners == "Lrnr_gam")]
learners <- learners[!(learners == "Lrnr_glmtree")]
learners <- learners[!(learners == "Lrnr_ranger")]
learners <- learners[!(learners == "Lrnr_hal9001")]
learners <- learners[!(learners == "Lrnr_grfcate")]

# remove LightGBM on Windows
if (Sys.info()["sysname"] == "Windows") {
  learners <- learners[!(learners == "Lrnr_lightgbm")]
}

# test all relevant learners
lapply(learners, test_learner_with_formula, task)
