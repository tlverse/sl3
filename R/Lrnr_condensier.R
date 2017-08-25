## ------------------------------------------------------------------------
## Learner for fitting conditional density using "condensier" R package
## ------------------------------------------------------------------------

#' @export
#' @rdname undocumented_learner
Lrnr_condensier <- R6Class(classname = "Lrnr_condensier", inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(bin_method = c("equal.mass", "equal.len", "dhist"),
                          nbins = NA_integer_,
                          max_n_cat = 20,
                          pool = FALSE,
                          max_n_bin = 1000,
                          bin_estimator = Lrnr_glm_fast$new(family = "binomial"),
                          ...) {

      assert_that(is(bin_estimator, "Lrnr_base") || is(bin_estimator, "logisfitR6"))

      ## Perform additional injection if bin_estimator is a learner from sl3 package.
      ## Wrap sl3 learner object into special wrapper class that
      ## provides a communication link between the two packages.
      if (inherits(bin_estimator,"Lrnr_base")) {
        bin_estimator <- Lrnr_pkg_condensier_logisfitR6$new(sl3_lrnr = bin_estimator)
      }

      params <- list(
        bin_estimator = bin_estimator,
        bin_method = bin_method[1L],
        nbins = nbins[1L],
        max_n_cat = max_n_cat,
        pool = pool,
        max_n_bin = max_n_bin, 
        ...)

      super$initialize(params = params, ...)
    }
  ),

  private = list(
    .covariates = NULL,
    .train = function(task) {
      verbose = getOption("sl3.verbose")
      params <- self$params
      private$.covariates <- task$nodes$covariates
      if ("covariates" %in% names(params)) {
        private$.covariates <- intersect(private$.covariates, params$covariates)
      }

      fit_object <- condensier::fit_density(
        X = private$.covariates,
        Y = task$nodes$outcome,
        input_data = task$data,
        nbins = params$nbins,
        bin_estimator = params$bin_estimator,
        pool = params$pool,
        verbose = verbose
      )
      return(fit_object)
    },

    .predict = function(task = NULL) {
      verbose = getOption("sl3.verbose")
      predictions <- condensier::predict_probability(private$.fit_object, task$data)
      # sampled_value <- condensier::sample_value(private$.fit_object, task$data)
      # predictions <- data.table::data.table(likelihood = predictions, sampled_value = sampled_value)
      predictions <- data.table::data.table(likelihood = predictions)
      return(predictions)
    }
), )


## ------------------------------------------------------------------------
## meta learner for density, find convex combo of candidate density estimators
## by minimizing the cross-validated negative log-likelihood loss of each density.
## The optimization problem is solved with Rsolnp::solnp, using largrandge multipliers.
## ------------------------------------------------------------------------
#' @export
#' @rdname undocumented_learner
Lrnr_solnp_density <- R6Class(classname = "Lrnr_solnp_density", inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(...) {
      params <- list(...)
      super$initialize(params = params)
    }
  ),

  private = list(
    .covariates = NULL,
    .train = function(task) {
      verbose = getOption("sl3.verbose")
      params <- self$params

      eval_fun_loss <- function(alphas) {
        sum(-log(as.vector(as.matrix(task$X) %*% alphas)))
      }

      eq_fun <- function(alphas) {
        sum(alphas)
      }

      fit_object <- Rsolnp::solnp(runif(ncol(task$X)), eval_fun_loss,  eqfun = eq_fun, eqB = 1, LB = rep(0L, ncol(task$X)))
      fit_object$coef <- fit_object$pars
      names(fit_object$coef) <- colnames(task$X)
      # if (verbose) {
        cat("\ndensity meta-learner fit:\n"); print(fit_object$coef)
      # }
      fit_object$name <- "solnp"
      return(fit_object)
    },

    .predict = function(task = NULL) {
      verbose <- getOption("sl3.verbose")
      X <- task$X
      predictions <- rep.int(NA, nrow(X))
      if (nrow(X) > 0) {
        coef <- private$.fit_object$coef
        if (!all(is.na(coef))) {
          eta <- as.matrix(X[, which(!is.na(coef)), drop = FALSE, with = FALSE]) %*% coef[!is.na(coef)]
          predictions <- eta
        }
      }
      return(data.table::data.table(predictions))
    }
), )