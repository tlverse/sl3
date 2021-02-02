context("test-gts.R -- Lrnr_gts for grouped time series forecasts")

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
grps <- rbind(c(1, 1, 2, 2), c(1, 2, 1, 2))
horizon <- 12
suppressWarnings(abc_long <- melt(abc, id = "time", variable.name = "series"))

# create sl3 task (no outcome for hierarchical/grouped series)
node_list <- list(outcome = "value", time = "time", id = "series")
train_task <- sl3_Task$new(data = abc_long, nodes = node_list)
test_data <- expand.grid(time = 51:55, series = unique(abc_long$series))
test_data <- as.data.table(test_data)[, value := 0]
test_task <- sl3_Task$new(data = test_data, nodes = node_list)

test_that("Lrnr_gts produces expected forecasts as gts implementation", {
  # compute forecast via sl3 learner
  gts_learner <- Lrnr_gts$new()
  gts_learner_fit <- gts_learner$train(train_task)
  gts_learner_preds <- gts_learner_fit$predict(test_task)

  # compute forecast via hts package
  gts_fit <- hts::gts(ts(as.matrix(abc[, -5])))
  train_hmax <- max(abc$time)
  test_hmax <- max(unique(test_data$time))
  gts_fpreds <- forecast(gts_fit, h = test_hmax - train_hmax)$bts
  gts_preds <- as.data.table(gts_fpreds)[, time := (train_hmax + 1):test_hmax]
  gts_preds <- melt(gts_preds, id.vars = "time", variable.name = "series")

  # predictions should be exactly the same
  expect_equal(gts_learner_preds, gts_preds$value)
})
