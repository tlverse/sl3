context("test-density-pooled_hazards: Lrnr_pooled_hazards + Lrnr_density_discretize")

# define test dataset
n <- 1e3
x <- rnorm(n)
epsilon <- rnorm(n)
y <- 3 * x + epsilon
data <- data.table(x = x, y = y)
task <- sl3_Task$new(data, covariates = c("x"), outcome = "y")

# reformat data into pooled hazards structure
hazards_task <- pooled_hazard_task(task)

# fit discrete density model to pooled hazards data
hse_learner <- make_learner(Lrnr_density_hse,
                            mean_learner = make_learner(Lrnr_glm_fast))
hse_fit <- hse_learner$train(task)

x_grid <- seq(from = min(data$x), to = max(data$x), length = 100)
y_grid <- seq(from = min(data$y), to = max(data$y), length = 100)
pred_data <- as.data.table(expand.grid(x = x_grid, y = y_grid))
pred_task <- make_sl3_Task(pred_data, covariates = c("x"), outcome = "y")

pred_data$dens_preds <- hse_fit$predict(pred_task)
pred_data[, true_dens := dnorm(x = y, mean = 3 * x)]
nll <- sum(-1 * pred_data$true_dens * log(pred_data$dens_preds))
expect_lt(nll, n)
