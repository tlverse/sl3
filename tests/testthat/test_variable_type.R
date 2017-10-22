context("test_variable_types.R -- Variable Type Handling")


# guess variable type
data(mtcars)
variable_types <- lapply(mtcars,function(x)variable_type(x=x,pcontinuous=0.25))
type_names <- sapply(variable_types, `[[`, "type")
expected_variable_types <- c(mpg = "continuous", cyl = "categorical", disp = "continuous", 
                             hp = "continuous", drat = "continuous", wt = "continuous", 
                             qsec = "continuous", vs = "binomial", am = "binomial", gear = "categorical", 
                             carb = "categorical")

test_that("variable_type makes good guesses", 
          expect_equal(type_names,expected_variable_types))


# forcing outcome_type
data(cpp)
cpp <- cpp[!is.na(cpp[, "haz"]), ]
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
cpp[is.na(cpp)] <- 0
# cpp <- cpp[sample(nrow(cpp),10000,replace=T),]
outcome <- "haz"
# cpp <- cpp[1:150, ]
# sl3_Task$debug("initialize")
task <- sl3_Task$new(cpp, covariates = covars, outcome = outcome, outcome_type = "fake_outcome_type")
test_that("outcome_type can be forced", 
          expect_equal(task$outcome_type$type, "fake_outcome_type"))

subtask <- task[1:10]
test_that("forced outcome_type is transferred on susbet", 
          expect_equal(subtask$outcome_type$type, "fake_outcome_type"))

chained_task <- task$next_in_chain(covariates = c("apgar1", "apgar5"))
test_that("forced outcome_type is transferred on chain", 
          expect_equal(chained_task$outcome_type$type, "fake_outcome_type"))

chained_task <- task$next_in_chain(outcome = "parity")
test_that("forced outcome_type is not transferred on chain if outcome changes", 
          expect_true(chained_task$outcome_type$type!="fake_outcome_type"))

task <- sl3_Task$new(cpp, covariates = covars, outcome = outcome, outcome_type = "categorical")
test_that("forcing outcome_type='categorical' generates outcome_levels", 
          expect_equal(task$outcome_type$levels, sort(unique(cpp$haz))))


task <- sl3_Task$new(cpp, covariates = covars, outcome = outcome, outcome_type = "continuous", outcome_levels=1:3)
test_that("outcome_levels can be forced", 
          expect_equal(task$outcome_type$levels, 1:3))

task <- sl3_Task$new(cpp, covariates = covars, outcome = outcome, outcome_type = "categorical")
Y_categorical <- task$outcome_type$format(task$Y)
test_that("outcome levels are passed as factor levels from format_Y",
          expect_equal(levels(Y_categorical), as.character(task$outcome_type$levels)))

Y_binomial <- variable_type("binomial", levels=levels(task$Y))$format(task$Y) 
test_that("outcome levels are binarized for outcome_type binomial",{
  expect_true(all(Y_binomial%in%c(0,1)))
  expect_equal(Y_binomial, as.numeric(task$Y==max(levels(task$Y))))
})
