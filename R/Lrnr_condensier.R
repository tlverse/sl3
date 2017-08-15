## ------------------------------------------------------------------------
## Learner for fitting conditional density using "condensier" R package
## ------------------------------------------------------------------------

#' @export
#' @rdname undocumented_learner
Lrnr_condensier <- R6Class(classname = "Lrnr_condensier", inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(bin.method = c("equal.mass", "equal.len", "dhist"),
                          nbins = NA_integer_,
                          maxncats = 20,
                          poolContinVar = FALSE,
                          maxNperBin = 1000,
                          bin_estimator = Lrnr_glm_fast$new(family = "binomial"),
                          ...) {

      assert_that(is(bin_estimator, "Lrnr_base") || is(bin_estimator, "logisfitR6"))

      params <- list(
        bin_estimator = bin_estimator,
        bin.method = bin.method[1L],
        nbins = nbins[1L],
        maxncats = maxncats,
        poolContinVar = poolContinVar,
        maxNperBin = maxNperBin,
        ...
      )

      super$initialize(params = params)
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
        verbose = TRUE
      )
      return(fit_object)
    },

    .predict = function(task = NULL) {
      verbose = getOption("sl3.verbose")
      predictions <- condensier::predict_probability(private$.fit_object, task$data)
      sampled_value <- condensier::sample_value(private$.fit_object, task$data)
      predictions <- data.table::data.table(likelihood = predictions, sampled_value = sampled_value)
      return(predictions)
    }
), )