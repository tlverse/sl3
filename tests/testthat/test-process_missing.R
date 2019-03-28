context("test-cpp_data.R -- cpp data and subsets")

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

data(cpp)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"

expect_warning(task_drop_missing <- make_sl3_Task(cpp, covars, outcome, drop_missing_outcome = TRUE),
               "Missing Covariate Data Found. Imputing covariates using sl3_process_missing")

expect_false(any(is.na(task_drop_missing$Y)))
expect_equal(task_drop_missing$nodes$covariates,
             c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", 
               "sexn", "delta_apgar1", "delta_apgar5", "delta_parity", "delta_meducyrs"
             ))

expect_warning({task_impute_covariates <- make_sl3_Task(cpp, covars, outcome)},
               "Missing Outcome Data Found. This is okay for prediction, but will likely break training. \n
               You can drop observations with missing outcomes by setting drop_missing_outcome=TRUE in make_sl3_Task")

expect_equal(task_impute_covariates$nrow, nrow(cpp))
