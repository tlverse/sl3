context("test-hts.R -- Lrnr_hts for hierarchical time series forecasts")

if (FALSE) {
  setwd("..")
  setwd("..")
  getwd()
  library("devtools")
  document()
  # load all R files in /R and datasets in /data. Ignores NAMESPACE:
  load_all("./")
  # devtools::check() # runs full check
  setwd("..")
  # INSTALL W/ devtools:
  install("sl3", build_vignettes = FALSE, dependencies = FALSE)
}

# Example adapted from hts package manual
# The hierarchical structure looks like 2 child nodes associated with level 1,
# which are followed by 3 and 2 sub-child nodes respectively at level 2.
library(hts)
set.seed(3274)
nodes <- list(2, c(3, 2))
abc <- as.data.table(5 + matrix(sort(rnorm(500)), ncol = 5, nrow = 100))
setnames(abc, paste("Series", 1:ncol(abc), sep = "_"))
horizon <- 12

# create sl3 task (no outcome for hierarchical/grouped series)
task <- sl3_Task$new(abc, covariates = colnames(abc))

test_that("Lrnr_hts produces expected forecasts as hts implementation", {
  # compute forecast via sl3 learner
  hts_learner <- Lrnr_hts$new(nodes = nodes, h = horizon)
  hts_learner_fit <- hts_learner$train(task)
  hts_learner_preds <- hts_learner_fit$predict(task)

  # compute forecast via hts package
  hts_fit <- hts::hts(ts(as.matrix(abc)), nodes)
  hts_preds <- forecast::forecast(hts_fit, h = horizon)
  hts_preds_grps <- hts::aggts(hts_preds)
  hts_preds_total <- as.numeric(hts_preds_grps[, "Total"])

  # predictions should be exactly the same
  expect_equal(hts_learner_preds, hts_preds_total)
})
