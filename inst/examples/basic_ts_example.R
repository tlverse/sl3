if (FALSE) {
  setwd("..")
  setwd("..")
  getwd()
  library("devtools")
  document()
  load_all("./") # load all R files in /R and datasets in /data. Ignores NAMESPACE:
  setwd("..")
  install("sl3", build_vignettes = FALSE, dependencies = FALSE) # INSTALL W/ devtools:
}

library(sl3)
library(data.table)
library(origami)

data(bsds)
covars <- c("cnt")
outcome <- "cnt"

load_all()

# Time-series problem with Recursive CV
folds <- make_folds(bsds$cnt, fold_fun = folds_rolling_origin, first_window = 500, validation_size = 50, gap = 0, batch = 50)
task <- sl3_Task$new(bsds, covariates = covars, outcome = outcome, folds = folds)
task$nodes$covariates

####################################
# Cross-validate simple ARIMA model:
arima_auto <- Lrnr_arima$new(order = c(1, 1, 1))
arima_auto_cv <- Lrnr_cv$new(arima_auto)
cv_fit <- arima_auto_cv$train(task)

# There should be 4 different fits:
cv_fit$fit_object$fold_fits

# Corresponding CV folds:
cv_fit$fit_object$folds

# Obtain predictions:
# NOTE: auto.arima DOES NOT WORK WITH Lrnr_cv! For some reason the fit returned complains with predict().
cv_fit$predict()

####################################
# Cross-validate simple GARCH model:
garch_1 <- Lrnr_rugarch$new(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
  distribution.model = "norm"
)
garch_1_cv <- Lrnr_cv$new(garch_1)
cv_fit <- garch_1_cv$train(task)

# There should be 4 different fits:
cv_fit$fit_object$fold_fits

# Corresponding CV folds:
cv_fit$fit_object$folds

# Obtain predictions:
cv_fit$predict()

#####################################################################################
# now lets stack some learners

arima_AR1 <- Lrnr_arima$new(order = c(1, 1, 1))
arima_AR2 <- Lrnr_arima$new(order = c(2, 1, 1))
arima_MA2 <- Lrnr_arima$new(order = c(1, 1, 2))
garch_1 <- Lrnr_rugarch$new(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
  distribution.model = "norm"
)
garch_2 <- Lrnr_rugarch$new(
  variance.model = list(model = "sGARCH", garchOrder = c(2, 2)),
  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
  distribution.model = "norm"
)

learner_stack <- Stack$new(arima_AR1, arima_AR2, arima_MA2, garch_1, garch_2)
cv_stack <- Lrnr_cv$new(learner_stack)
cv_fit <- cv_stack$train(task)

# There should be 4 different fits:
cv_fit$fit_object$fold_fits

# Corresponding CV folds:
cv_fit$fit_object$folds

# Obtain predictions:
cv_fit$predict()
