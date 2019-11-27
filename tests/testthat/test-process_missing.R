context("test-process_missing.R -- Missing Handling for sl3_Task objects")

if (FALSE) {
  setwd("..")
  setwd("..")
  getwd()
  library("devtools")
  document()
  load_all("./") # load all R files in /R and datasets in /data. Ignores NAMESPACE:
  setwd("..")
  install("sl3",
    build_vignettes = FALSE,
    dependencies = FALSE
  ) # INSTALL W/ devtools:
}

data(cpp)
covars <- c(
  "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
  "sexn"
)
outcome <- "haz"

warnings <- capture_warnings({
  task_drop_missing <- make_sl3_Task(cpp, covars, outcome,
    drop_missing_outcome = TRUE
  )
})

expect_equal(
  warnings,
  c(
    "Missing outcome data detected: dropping outcomes.",
    "Missing covariate data detected: imputing covariates."
  )
)

expect_false(any(is.na(task_drop_missing$Y)))
expect_equal(
  task_drop_missing$nodes$covariates,
  c(
    "apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
    "sexn", "delta_apgar1", "delta_apgar5", "delta_parity", "delta_meducyrs"
  )
)

warnings <- capture_warnings({
  task_impute_covariates <- make_sl3_Task(cpp, covars, outcome)
})
expect_equal(
  warnings,
  c(
    "Missing covariate data detected: imputing covariates.",
    "Missing outcome data detected. This is okay for prediction, but will likely break training. \n You can drop observations with missing outcomes by setting drop_missing_outcome=TRUE in make_sl3_Task."
  )
)

expect_equal(task_impute_covariates$nrow, nrow(cpp))

# create data with missingness
mtcars_with_missing <- data.table(copy(mtcars))
mtcars_with_missing[sample(1:nrow(mtcars),10),cyl:=NA]

# also add character column
mtcars_with_missing[,gear:=as.character(gear)]

# create a task specifing nodes
covariates <- c("cyl","gear")
suppressWarnings({
task_from_nodes <- sl3_Task$new(mtcars_with_missing, nodes = list(outcome="mpg", covariates=covariates))
})
expected_covariates <- c(covariates, "delta_cyl")

test_that("missing processing works when nodes is specified", 
          expect_equal(task_from_nodes$nodes$covariates, expected_covariates))
