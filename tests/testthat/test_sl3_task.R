library(testthat)
context("test_sl3_task -- Basic sl3_Task functionality")


library(sl3)
library(data.table)
library(uuid)


# define test dataset
data(mtcars)
covariates <- c(
  "cyl", "disp", "hp", "drat", "wt", "qsec",
  "vs", "am", "gear", "carb"
)
outcome <- "mpg"
task <- sl3_Task$new(mtcars, covariates = covariates, outcome = outcome)


test_that("task$X returns appropriate data", {
  X <- task$X
  expect_equal(dim(X), c(nrow(mtcars), length(covariates)))
  expect_equal(names(X), covariates)
})

test_that("task$Y returns appropriate data", {
  Y <- task$Y
  expect_length(Y, nrow(mtcars))
  expect_equal(Y, mtcars[[outcome]])
})

test_that("task$weights returns appropriate data", {
  weights <- task$weights
  expect_length(weights, nrow(mtcars))
  expect_true(all(weights == 1))
})

test_that("task subsetting works", {
  subset_vector <- 5:10
  subsetted <- task[subset_vector]
  expect_equal(subsetted$X, task$X[subset_vector])

  # we can double subset a task
  # (where the second subset vector indexes the logical rows of the first subset)
  subset_vector2 <- c(4, 6)
  subsetted_2 <- subsetted[subset_vector2]
  expected_rows <- subset_vector[subset_vector2]
  expect_equal(subsetted_2$Y, mtcars[[outcome]][expected_rows])

  # modifying a subset modifies the original
  column_map <- subsetted_2$add_columns("test_fit", new_data = data.table(data = 1:2))
  new_col_name <- tail(column_map, 1)[[1]]

  # extra column from original
  new_column <- task$raw_data[[new_col_name]]
  expect_equal(new_column[expected_rows], 1:2)
})

empty_task <- sl3_Task$new(mtcars, covariates = NULL, outcome = NULL)

test_that("task$X_intercept works for empty X (intercept-only)", {
  expect_equal(nrow(empty_task$X_intercept), nrow(mtcars))
})

test_that("task errors for empty Y", {
  expect_error(empty_task$Y)
})


test_that("two chained tasks can have the same column name without conflicts", {
  new_data1 <- task$data[, test_col := 1]
  column_names1 <- task$add_columns(fit_uuid = UUIDgenerate(), new_data = new_data1)
  chained1 <- task$next_in_chain(covariates = "test_col", column_names = column_names1)

  new_data2 <- task$data[, test_col := 2]
  column_names2 <- task$add_columns(fit_uuid = UUIDgenerate(), new_data = new_data2)
  chained2 <- task$next_in_chain(covariates = "test_col", column_names = column_names2)

  expect_true(all(chained1$X$test_col == 1))
  expect_true(all(chained2$X$test_col == 2))
})
