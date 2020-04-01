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
grps <- rbind(c(1, 1, 2, 2), c(1, 2, 1, 2))
horizon <- 12

# create sl3 task (no outcome for hierarchical/grouped series)
task <- sl3_Task$new(abc, covariates = colnames(abc))

test_that("Lrnr_gts produces expected forecasts as gts implementation", {
  # compute forecast via sl3 learner
  gts_learner <- Lrnr_gts$new(groups = grps, h = horizon)
  gts_learner_fit <- gts_learner$train(task)
  gts_learner_preds <- gts_learner_fit$predict(task)

  # compute forecast via hts package
  gts_fit <- hts::gts(ts(as.matrix(abc)), grps)
  all_gts_fits <- aggts(gts_fit)
  all_gts_forecasts <- lapply(seq_len(ncol(all_gts_fits)), function(iter) {
    forecast(all_gts_fits[, iter], h = horizon)$mean
  })
  gts_forecast_total <-
    all_gts_forecasts[[which(colnames(all_gts_fits) == "Total")]]
  gts_preds_total <- as.numeric(gts_forecast_total)

  # predictions should be exactly the same
  expect_equal(gts_learner_preds, gts_preds_total)
})
