## ------------------------------------------------------------------------
## Learner for fitting conditional density using "condensier" R package
## ------------------------------------------------------------------------

#' @export
#' @rdname undocumented_learner
Lrnr_condensier <- R6Class(classname = "Lrnr_condensier", inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(bin_method = c("equal.mass", "equal.len", "dhist"),
                          nbins = 5,
                          max_n_cat = 20,
                          pool = FALSE,
                          max_n_bin = NA_integer_,
                          parfit = FALSE,
                          bin_estimator = Lrnr_glm_fast$new(family = "binomial"),
                          intrvls = NULL,
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
        parfit = parfit,
        intrvls = intrvls,
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
        bin_method = params$bin_method,
        nbins = params$nbins,
        bin_estimator = params$bin_estimator,
        pool = params$pool,
        max_n_bin = params$max_n_bin,
        parfit = params$parfit,
        intrvls = params$intrvls,
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
    },
    .required_packages = c("condensier")
), )