#' Conditional Density Estimation
#'
#' This learner provides facilities for conditional density estimation using
#' the \code{condensier} package. Fitting is done with the
#' \code{\link[condensier]{fit_density}} function.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stringr str_extract str_remove
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
#'  \item{\code{bin_method=c("equal.mass", "equal.len", "dhist")}}{The type of
#'   smoothing to be performed. See documentation of the \code{condensier}
#'   package for details.}
#'  \item{\code{nbins=5}}{The number of observations per bin. See documentation
#'   of the \code{condensier} package for details.}
#'  \item{\code{max_n_cat = 20}}{Maximum number of unique levels for categorical
#'   outcomes. See documentation of the \code{condensier} package for details.}
#'  \item{\code{pool = FALSE}}{Whether pooling of data across bins should be
#'   performed. See documentation of the \code{condensier} package for details.}
#'  \item{\code{max_n_bin=NA}}{Maximum number of observations per single bin for
#'   continuous outcome. See documentation of the \code{condensier} package for
#'   details.}
#'  \item{\code{parfit=FALSE}}{Whether to invoke parallelization in the fitting
#'   procedure. See documentation of the \code{condensier} package for details.}
#'  \item{\code{bin_estimator=make_learner(Lrnr_glm_fast, family=binomial())}}{
#'   The classification algorithm to be used in the fitting process. See
#'   documentation of the \code{condensier} package for details.}
#'  \item{\code{intrvls=NULL}}{An interval range to be used for custom bin
#'   definitions. See documentation of the \code{condensier} package for
#'   details.}
#'  \item{\code{type=c("probability", "sampled")}}{Whether to return predicitons
#'   that are probabilities or values sampled from the estimated conditional
#'   density. Refer to \code{condensier::predict_probability} for the former
#'   and \code{condensier::sample_value} for the latter option.}
#' }
#'
#' @template common_parameters
#
Lrnr_condensier <- R6Class(
  classname = "Lrnr_condensier",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(bin_method = c("equal.mass", "equal.len", "dhist"),
                              nbins = 5,
                              max_n_cat = 20,
                              pool = FALSE,
                              max_n_bin = NA_integer_,
                              parfit = FALSE,
                              bin_estimator = make_learner(
                                Lrnr_glm_fast,
                                family = binomial()
                              ),
                              intrvls = NULL,
                              type = "probability",
                              ...) {
      params <- args_to_list()
      assert_that(is(bin_estimator, "Lrnr_base") || is(
        bin_estimator,
        "logisfitR6"
      ))
      ## Perform additional injection if bin_estimator is a learner from sl3.
      ## Wrap sl3 learner object into special wrapper class that
      ## provides a communication link between the two packages.
      if (inherits(bin_estimator, "Lrnr_base")) {
        params$bin_estimator <-
          Lrnr_pkg_condensier_logisfitR6$new(sl3_lrnr = bin_estimator)
      }
      super$initialize(params = params, ...)
    }
  ),

  active = list(
    name = function() {
      # TODO: allow custom names
      if (is.null(private$.name)) {
        params <- self$params
        if (length(params) > 0) {
          # TODO: sanitize further
          atom_params <- sapply(params, is.atomic)
          bin_lrnr_name <- str_remove(str_extract(
            params$bin_estimator$lmclass,
            "::.+"
          ), "::")
          if (is.na(bin_lrnr_name)) {
            bin_lrnr_name <- params$bin_estimator$lmclass
          }
          params <- params[atom_params]
          params$bin_estimator <- bin_lrnr_name
        }
        props <- c(list(class(self)[1]), params)
        name <- paste(props, collapse = "_")
        private$.name <- name
      }
      return(private$.name)
    }
  ),

  private = list(
    .properties = c("density", "continuous", "weights"),
    .covariates = NULL,
    .train = function(task) {
      verbose <- getOption("sl3.verbose")
      args <- self$params
      args$X <- task$nodes$covariates
      args$Y <- task$nodes$outcome
      if (task$has_node("weights")) {
        args$weights <- task$weights
      }
      args$input_data <- task$data
      fit_object <- call_with_args(condensier::fit_density, args)
      return(fit_object)
    },

    .predict = function(task = NULL) {
      verbose <- getOption("sl3.verbose")
 
      # get prediction type
      type <- self$params$type
      type <- match.arg(type, choices = c("probability", "sampled"))

      if (type == "probability") {
        predictions <- condensier::predict_probability(
          private$.fit_object,
          task$data
        )
        predictions <- data.table::data.table(likelihood = predictions)
      } else if (type == "sampled") {
        sampled_value <- condensier::sample_value(
          private$.fit_object,
          task$data
        )
        predictions <- data.table::data.table(sampled_value = sampled_value)
      }
      return(predictions)
    },
    .required_packages = c("condensier")
  )
)
