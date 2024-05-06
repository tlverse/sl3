context("test-hts.R -- Lrnr_hts for hierarchical time series forecasts")
skip()
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

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
abc <- as.data.table(5 + matrix(sort(rnorm(200)), ncol = 4, nrow = 50))
setnames(abc, paste("Series", 1:ncol(abc), sep = "_"))
abc[, time := .I]
nodes <- list(2, c(3, 2))
horizon <- 12
suppressWarnings(abc_long <- melt(abc, id = "time", variable.name = "series"))

# create sl3 task (no outcome for hierarchical/grouped series)
node_list <- list(outcome = "value", time = "time", id = "series")
train_task <- sl3_Task$new(data = abc_long, nodes = node_list)
test_data <- expand.grid(time = 51:55, series = unique(abc_long$series))
test_data <- as.data.table(test_data)[, value := 0]
test_task <- sl3_Task$new(data = test_data, nodes = node_list)

test_that("Lrnr_hts produces expected forecasts as hts implementation", {
  # compute forecast via sl3 learner
  hts_learner <- Lrnr_hts$new()
  hts_learner_fit <- hts_learner$train(train_task)
  hts_learner_preds <- hts_learner_fit$predict(test_task)

  # compute forecast via hts package
  hts_fit <- hts::hts(ts(as.matrix(abc)[, -5]))
  train_hmax <- max(abc$time)
  test_hmax <- max(unique(test_data$time))
  hts_fpreds <- forecast(hts_fit, h = test_hmax - train_hmax)$bts
  hts_preds <- as.data.table(hts_fpreds)[, time := (train_hmax + 1):test_hmax]
  hts_preds <- melt(hts_preds, id.vars = "time", variable.name = "series")

  # predictions should be exactly the same
  expect_equal(hts_learner_preds, hts_preds$value)
})
