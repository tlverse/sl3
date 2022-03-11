context("test_glm_semiparametric.R -- Lrnr_glm_semiparametric")
library(glmnet)

set.seed(459)
n <- 200
W <- runif(n, -1, 1)
A <- rbinom(n, 1, plogis(W))
Y_continuous <- rnorm(n, mean = A + W, sd = 0.3)
Y_binary <- rbinom(n, 1, plogis(A + W))
Y_count <- rpois(n, exp(A + W))
data <- data.table::data.table(W, A, Y_continuous, Y_binary, Y_count)

# Make tasks
task_continuous <- sl3_Task$new(
  data, covariates = c("A", "W"), outcome = "Y_continuous"
)
task_binary <- sl3_Task$new(
  data, covariates = c("A", "W"), outcome = "Y_binary"
)
task_count <- sl3_Task$new(
  data, covariates = c("A", "W"), outcome = "Y_count", 
  outcome_type = "continuous"
)
 
formula_sp <- ~ 1 + W

# fit partially-linear regression with append_interaction_matrix = TRUE
set.seed(100)
lrnr_glm_sp_gaussian <- Lrnr_glm_semiparametric$new(
  formula_sp = formula_sp, family =  gaussian(), 
  lrnr_baseline =  Lrnr_glmnet$new(), 
  interaction_variable = "A", append_interaction_matrix = TRUE
)
lrnr_glm_sp_gaussian <- lrnr_glm_sp_gaussian$train(task_continuous)
preds <- lrnr_glm_sp_gaussian$predict(task_continuous)
beta <- lrnr_glm_sp_gaussian$fit_object$coefficients
# in this case, since append_interaction_matrix = TRUE, it is equivalent to:
V <- model.matrix(formula_sp, task_continuous$data)
X <- cbind(task_continuous$data[["W"]], task_continuous$data[["A"]] * V)
X0 <- cbind(task_continuous$data[["W"]], 0 * V)
colnames(X) <- c("W", "A", "A*W")
Y <- task_continuous$Y
set.seed(100)
beta_equiv <- coef(cv.glmnet(X,Y,family="gaussian"),s="lambda.min")[c(3,4)]
# actually, the glmnet fit is projected onto the semiparametric model 
# with glm.fit, no effect in this case
test_that("Equivalence of beta when append_interaction_matrix = TRUE", {
  expect_equal(as.numeric(beta), beta_equiv)
})

# fit partially-linear regression w append_interaction_matrix = FALSE`
set.seed(100)
lrnr_glm_sp_gaussian <- Lrnr_glm_semiparametric$new(
  formula_sp = formula_sp, family = gaussian(), 
  lrnr_baseline = Lrnr_glm$new(family = gaussian()), 
  interaction_variable = "A", 
  append_interaction_matrix = FALSE
)
lrnr_glm_sp_gaussian <- lrnr_glm_sp_gaussian$train(task_continuous)
preds <- lrnr_glm_sp_gaussian$predict(task_continuous)
beta <- lrnr_glm_sp_gaussian$fit_object$coefficients
# in this case, since append_interaction_matrix = FALSE, it is equivalent to
# the following
cntrls <- task_continuous$data[["A"]] == 0 # subset to control arm
V <- model.matrix(formula_sp, task_continuous$data)
X <- cbind(rep(1, n), task_continuous$data[["W"]])
Y <- task_continuous$Y
set.seed(100)
beta_Y0W <- lrnr_glm_sp_gaussian$fit_object$lrnr_baseline$fit_object$coefficients
# subset to treatment arm
beta_Y0W_equiv <- coef(
   glm.fit(X[cntrls, , drop = F], Y[cntrls], family = gaussian())
)
EY0 <- X %*% beta_Y0W
beta_equiv <- coef(glm.fit(A * V, Y, offset = EY0, family = gaussian()))
test_that("Equivalence of beta_Y0W when append_interaction_matrix = FALSE", {
  expect_equal(as.numeric(beta_Y0W), beta_Y0W_equiv)
})
test_that("Equivalence of beta when append_interaction_matrix = FALSE", {
  expect_equal(as.numeric(beta), as.numeric(beta_equiv))
})

# fit partially-linear logistic regression
lrnr_glm_sp_binomial <- Lrnr_glm_semiparametric$new(
  formula_sp = formula_sp, family = binomial(), 
  lrnr_baseline = Lrnr_glmnet$new(), interaction_variable = "A", 
  append_interaction_matrix = TRUE
)
lrnr_glm_sp_binomial <- lrnr_glm_sp_binomial$train(task_binary)
preds <- lrnr_glm_sp_binomial$predict(task_binary)
beta <- lrnr_glm_sp_binomial$fit_object$coefficients

# fit partially-linear log-link (relative-risk) regression
# Lrnr_glmnet$new(family = "poisson") setting requires that lrnr_baseline 
# predicts nonnegative values. It is recommended to use poisson 
# regression-based learners.
lrnr_glm_sp_binomial <- Lrnr_glm_semiparametric$new(
  formula_sp = formula_sp, family = poisson(), 
  lrnr_baseline = Lrnr_glmnet$new(family = "poisson"), 
  interaction_variable = "A", 
  append_interaction_matrix = TRUE
)
lrnr_glm_sp_binomial <- lrnr_glm_sp_binomial$train(task_count)
preds <- lrnr_glm_sp_binomial$predict(task_count)
beta <- lrnr_glm_sp_binomial$fit_object$coefficients
