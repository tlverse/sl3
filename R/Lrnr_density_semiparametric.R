#' Density Estimation With Mean Model and Homoscedastic Errors
#'
#' This learner assumes a mean model with homoscedastic errors: Y ~ E(Y|W) + epsilon. E(Y|W) is fit using any mean learner,
#' and then the errors are fit with kernel density estimation.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @export
#'
#' @keywords data
#'
#' @return Learner object with methods for training and prediction. See
#'  \code{\link{Lrnr_base}} for documentation on learners.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @family Learners
#'
#' @section Parameters:
#' \describe{
#'   \item{\code{binomial_learner}}{The learner to wrap.}
#' }
#'
#' @template common_parameters
#'
#' @examples
#' # load example data
#' data(cpp_imputed)
#'
#' # create sl3 task
#' task <- sl3_Task$new(
#'   cpp_imputed,
#'   covariates = c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs"),
#'   outcome = "haz"
#' )
#'
#' # train density hse learner and make predictions
#' lrnr_density_semi <- Lrnr_density_semiparametric$new(
#'   mean_learner = Lrnr_glm$new()
#' )
#' lrnr_density_semi_fit <- lrnr_density_semi$train(task)
#' lrnr_density_semi_pred <- lrnr_density_semi_fit$predict()
Lrnr_density_semiparametric <- R6Class(
  classname = "Lrnr_density_semiparametric",
  inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(mean_learner = NULL, var_learner = NULL, bw = NULL, ...) {
      params <- args_to_list()

      if (is.null(params$mean_learner)) {
        params$mean_learner <- make_learner(Lrnr_glm_fast)
      }

      super$initialize(params = params, ...)
    },
    sample = function(task, n_samples = 30, fold_number = "full") {
      # TODO: fold
      # method: inverse cdf
      # style: parallel
      mean_fit <- self$fit_object$mean_fit
      var_fit <- self$fit_object$var_fit
      dens_fit <- self$fit_object$dens_fit

      mean_preds <- mean_fit$predict(task)
      if (!is.null(var_fit)) {
        var_preds <- var_fit$predict(task)

        var_preds[var_preds < 0] <- self$fit_object$min_obs_error
        sd_preds <- sqrt(var_preds)
      } else {
        sd_preds <- rep(1, task$nrow)
      }

      samples <- inverse_sample(n_samples, dens_fit)
      obs_samples <- sapply(
        1:task$nrow,
        function(i) (samples + mean_preds[i]) / sd_preds[i]
      )

      return(t(obs_samples))
    },
    get_outcome_range = function(task = NULL, fold_number = "full") {
      # TODO: fold
      mean_fit <- self$fit_object$mean_fit
      dens_fit <- self$fit_object$dens_fit

      if (!is.null(task)) {
        mean_preds <- mean_fit$predict(task)
        minimum <- min(dens_fit$x) + mean_preds
        maximum <- max(dens_fit$x) + mean_preds
        range <- cbind(minimum, maximum)
      } else {
        mean_preds <- mean_fit$predict(self$training_task)
        minimum <- min(dens_fit$x) + min(mean_preds)
        maximum <- max(dens_fit$x) + max(mean_preds)
        range <- c(minimum, maximum)
      }
      return(range)
    }
  ),
  private = list(
    .properties = c("density", "sampling"),
    .train = function(task) {
      mean_learner <- self$params$mean_learner
      var_learner <- self$params$var_learner
      bw <- self$params$bw

      if (is.null(bw)) {
        bw <- "nrd0"
      }
      mean_fit <- mean_learner$train(task)

      # todo: maybe these should be cv errors?
      mean_preds <- mean_fit$predict()
      errors <- task$Y - mean_preds

      if (!is.null(var_learner)) {
        new_columns <- task$add_columns(data.table(squared_error = errors^2))
        se_task <- task$next_in_chain(column_names = new_columns, outcome = "squared_error")
        min_obs_error <- 2 * min(se_task$Y)
        var_fit <- var_learner$train(se_task)
        var_preds <- var_fit$predict()
        
        # calculate a practical floor for error variance, called squared tolerance
        errors_sd <- sd(errors)
        tol2   <- (0.1 * errors_sd)^2  
        
        # replace any NA or too-small var_pred with tol2
        var_preds_clean <- ifelse(
          is.na(var_preds) | var_preds < 0,
          tol2,
          var_preds
        )
        sd_preds <- sqrt(var_preds_clean)
        errors <- errors / sd_preds
      } else {
        var_fit <- NULL
        min_obs_error <- NA
      }



      dens_fit <- density(errors, bw = bw)
      fit_object <- list(
        mean_fit = mean_fit,
        var_fit = var_fit,
        min_obs_error = min_obs_error,
        dens_fit = dens_fit
      )

      return(fit_object)
    },
    .predict = function(task) {
      mean_fit <- self$fit_object$mean_fit
      var_fit <- self$fit_object$var_fit
      dens_fit <- self$fit_object$dens_fit
      mean_preds <- mean_fit$predict(task)
      errors <- task$Y - mean_preds
      if (!is.null(var_fit)) {
        var_preds <- var_fit$predict(task)

        var_preds[var_preds < 0] <- self$fit_object$min_obs_error
        sd_preds <- sqrt(var_preds)
      } else {
        sd_preds <- rep(1, task$nrow)
      }

      errors <- errors / sd_preds

      dens_preds <- approx(dens_fit$x, dens_fit$y, errors, rule = 2)$y

      dens_preds <- dens_preds / sd_preds

      # dens_preds[is.na(dens_preds)] <- 0
      return(dens_preds)
    }
  )
)
