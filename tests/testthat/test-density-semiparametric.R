context("test-density-semiparametric.R -- Lrnr_density_semiparametric")

test_that("Lrnr_density_semiparametric works", {
  set.seed(1234)

  # define test dataset
  n <- 1e6
  x <- runif(n, 0, 3)
  epsilon_x <- rnorm(n, 0, 0.5 + sqrt(x))
  # epsilon_x <- rnorm(n)
  y <- 3 * x + epsilon_x

  data <- data.table(x = x, x2 = x^2, y = y)
  covariates <- c("x")
  task <- make_sl3_Task(data, covariates = covariates, outcome = "y")

  # train
  hse_learner <- make_learner(Lrnr_density_semiparametric,
    mean_learner = make_learner(Lrnr_glm)
  )

  mvd_learner <- make_learner(Lrnr_density_semiparametric,
    mean_learner = make_learner(Lrnr_glm),
    var_learner = make_learner(Lrnr_glm)
  )

  hse_fit <- hse_learner$train(task)
  mvd_fit <- mvd_learner$train(task)

  # test sampling
  y_samp <- mvd_fit$sample(task[1:10], 100)

  x_grid <- seq(from = min(data$x), to = max(data$x), length = 100)
  y_grid <- seq(from = min(data$y), to = 1.5 * max(data$y), length = 100)
  pred_data <- as.data.table(expand.grid(x = x_grid, y = y_grid))
  pred_data$x2 <- pred_data$x^2
  pred_task <- make_sl3_Task(pred_data, covariates = covariates, outcome = "y")

  pred_data$hse_preds <- hse_fit$predict(pred_task)
  pred_data$mvd_preds <- mvd_fit$predict(pred_task)
  pred_data[, true_dens := dnorm(x = y, mean = 3 * x, sd = abs(x))]

  nll <- function(observed, pred) {
    res <- -1 * observed * log(pred)
    res[observed < .Machine$double.eps] <- 0

    return(res)
  }

  hse_nll <- sum(nll(pred_data$true_dens, pred_data$hse_preds))
  mvd_nll <- sum(nll(pred_data$true_dens, pred_data$mvd_preds))

  expect_lt(hse_nll, n)
  expect_lt(mvd_nll, hse_nll)
  # long <- melt(pred_data, id = c("x", "y", "true_dens"), measure = c("hse_preds", "mvd_preds", "true_dens"))
  # x_samp <- sample(x_grid, 20)
  # ggplot(long[x%in%x_samp],aes(x=y,y=value,color=variable))+geom_line()+facet_wrap(~round(x,5),scales="free_x")+theme_bw()+coord_flip()
})


# Densities should integrate to 1 --- 
# 
#
# the core logic of this test is that a (conditional) density model fit by
# Lrnr_density_semiparametric (given predictors) should integrate to 1 as it
# should be an estimate of a probability density function.
# 
# 
test_that("Lrnr_density_semiparametric produces densities that integrate to 1", {
  
  # fit a heteroskedastic density model (using glm for the conditional mean
  # component, and glm for the conditional variance component), fix a vector of
  # covariates, and integrate the density across a region of plausible outcome
  # values.
  # 
  # this is in a function so we can repeat the test on at least two different
  # datasets.
  fit_a_heteroskedastic_density_model_and_integrate_area_under_density_curve <-
    function(
      df,
      covariates,
      outcome) {
      
    task <- sl3_Task$new(
      df,
      covariates = covariates,
      outcome = outcome)
    
    heteroskedastic_glm_glm_sl3 <- Lrnr_density_semiparametric$new(
      mean_learner = make_learner(Lrnr_glm),
      var_learner = make_learner(Lrnr_glm)
    )
    
    Lrnr_glm_heteroskedastic_fit <- heteroskedastic_glm_glm_sl3$train(task)
    Lrnr_glm_heteroskedastic_predictor <- Lrnr_glm_heteroskedastic_fit$predict
    
    
    f_heteroskedastic_density <- function(ys) {
      # take the first row to use to fix all the covariates
      x <- df[1,,drop=FALSE]
      # replicate it for each point at which we will evaluate it to integrate it 
      newdata <- dplyr::bind_rows(replicate(n = length(ys), expr = x, simplify = FALSE))
      # replace the outcome with each of the values to evaluate the density at
      newdata[outcome] <- ys
      
      new_task <- sl3::sl3_Task$new(
        data = newdata,
        covariates = covariates,
        outcome = outcome)
        
      Lrnr_glm_heteroskedastic_predictor(new_task)
    }
    
    # setup a range across which we will integrate
    lower_than_min <- min(df[[outcome]]) - 10*sd(df[[outcome]])
    higher_than_max <- max(df[[outcome]]) + 10*sd(df[[outcome]])
    
    # plotting code for a sanity/face-value check:
    # outcome_values <- seq(lower_than_min, higher_than_max, length.out = 10000)
    # density_values <- f_heteroskedastic_density(outcome_values)
    # plot(outcome_values, density_values, type = 'l')
    
    # integrate the density given the covariates in the first row of df
    integrated_area_under_density_curve <-
      integrate(f_heteroskedastic_density,
                lower = lower_than_min,
                upper = higher_than_max)
    
    return(integrated_area_under_density_curve$value)
  }
    
  # the following test fits a conditional density model to the mtcars dataset 
  # using hp as the outcome and the other continuous variables 
  # as the covariates.
  
  mtcars_integrated_area_under_density <-
    fit_a_heteroskedastic_density_model_and_integrate_area_under_density_curve(
      df = mtcars,
      covariates = c('mpg', 'disp', 'drat', 'wt', 'qsec'),
      outcome = 'hp'
    )
  
  testthat::expect_gt(mtcars_integrated_area_under_density, .9)
  testthat::expect_lt(mtcars_integrated_area_under_density, 1.1)
  
  # the following test fits a conditional density model to the MASS::Boston
  # dataset using medv as the outcome and other continuous variables as the
  # covariates.
  # 
  # sl3 imports ggplot2 and ggplot2 imports MASS so no additional dependency 
  # is added by relying on MASS::Boston in this test. 
  Boston_integrated_area_under_density <-
    fit_a_heteroskedastic_density_model_and_integrate_area_under_density_curve(
      df = MASS::Boston,
      covariates = c('crim', 'indus', 'nox', 'rm', 'age', 'dis', 'tax'),
      outcome = 'medv'
    )
  
  testthat::expect_gt(Boston_integrated_area_under_density, .9)
  testthat::expect_lt(Boston_integrated_area_under_density, 1.1)
  
  
})

