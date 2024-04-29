context("test-sl3_task -- Basic sl3_Task functionality")
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
  expect_equal(subsetted$nrow, length(subset_vector))

  # we can double subset a task
  # (where the second subset vector indexes the logical rows of the first subset)
  subset_vector2 <- c(4, 6)
  subsetted_2 <- subsetted[subset_vector2]
  expected_rows <- subset_vector[subset_vector2]
  expect_equal(subsetted_2$Y, task$Y[expected_rows])
  expect_equal(subsetted_2$nrow, length(subset_vector2))

  # modifying a subset modifies the original
  column_map <- subsetted_2$add_columns(
    data.table(data = 1:2),
    "test_fit"
  )
  new_col_name <- tail(column_map, 1)[[1]]

  # extra column from original
  new_column <- unlist(task$internal_data$get_data(NULL, new_col_name),
    use.names = FALSE
  )
  expect_equal(new_column[expected_rows], 1:2)

  # logical subset vector
  logical_subset <- as.logical(rbinom(task$nrow, 1, 0.5))
  subsetted3 <- task[logical_subset]
  expect_equal(subsetted3$nrow, sum(logical_subset))
})

test_that("task$X_intercept works for empty X (intercept-only)", {
  empty_task <- sl3_Task$new(mtcars, covariates = NULL, outcome = NULL)
  expect_equal(nrow(empty_task$X_intercept), nrow(mtcars))
})

test_that("task errors for empty Y", {
  empty_task <- sl3_Task$new(mtcars, covariates = NULL, outcome = NULL)
  expect_error(empty_task$Y)
})

test_that("two chained tasks can have same column name without conflicts", {
  new_data1 <- data.table(test_col = 1)
  column_names1 <- task$add_columns(new_data1)

  chained1 <- task$next_in_chain(
    covariates = "test_col",
    column_names = column_names1
  )

  new_data2 <- data.table(test_col = 2)
  column_names2 <- task$add_columns(new_data2)

  chained2 <- task$next_in_chain(
    covariates = "test_col",
    column_names = column_names2
  )

  expect_true(all(chained1$X$test_col == 1))
  expect_true(all(chained2$X$test_col == 2))
})

test_that("integers can be passed as number of folds in V-fold CV", {
  task_intfolds <- sl3_Task$new(mtcars, covariates, outcome, folds = 4)
  expect_equal(length(task_intfolds$folds), 4)
})

test_that("task can be made from shared data", {
  task_from_shared_data <- sl3_Task$new(
    task$.__enclos_env__$private$.shared_data,
    nodes = task$nodes
  )
  expect_equal(task_from_shared_data$data, task$data)
})

test_that("mismatch of supplied and detected outcome_type warns", {
  expect_warning(
    sl3_Task$new(mtcars, covariates, outcome, outcome_type = "binomial")
  )
})

test_that("outcome_type gaussian/guassian() silently changed to continuous", {
  task <- sl3_Task$new(mtcars, covariates, outcome, outcome_type = "gaussian")
  task2 <- sl3_Task$new(mtcars, covariates, outcome, outcome_type = gaussian())
  expect_equal(
    c(task$outcome_type$type, task2$outcome_type$type),
    c("continuous", "continuous")
  )
})

test_that("outcome_type binary/binomial() silently changed to binomial", {
  mtcars$binary_mpg <- ifelse(mtcars$mpg > mean(mtcars$mpg), 1, 0)
  task <- sl3_Task$new(mtcars, covariates, "binary_mpg", outcome_type = "binary")
  task2 <- sl3_Task$new(mtcars, covariates, "binary_mpg", outcome_type = binomial())
  expect_equal(
    c(task$outcome_type$type, task2$outcome_type$type),
    c("binomial", "binomial")
  )
})

test_that("outcome_type multinomial is silently changed to categorical", {
  mtcars$categorical_mpg <- as.factor(
    ifelse(mtcars$mpg < 15, "L", ifelse(mtcars$mpg > 25, "H", "M"))
  )
  task <- sl3_Task$new(
    mtcars, covariates, "categorical_mpg",
    outcome_type = "multinomial"
  )
  expect_equal(task$outcome_type$type, "categorical")
})

test_that("erroneous outcome_type errors", {
  expect_error(
    sl3_Task$new(mtcars, covariates, outcome, outcome_type = "kitty")
  )
})

test_that("vector outcome_type errors", {
  expect_error(
    sl3_Task$new(mtcars, covariates, outcome, outcome_type = c("go", "bears"))
  )
})
