#' Conditional Density Estimation with the Highly Adaptive LASSO
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#'
#' @export
#'
#' @keywords data
#'
#' @return A learner object inheriting from \code{\link{Lrnr_base}} with
#'  methods for training and prediction. For a full list of learner
#'  functionality, see the complete documentation of \code{\link{Lrnr_base}}.
#'
#' @format An \code{\link[R6]{R6Class}} object inheriting from
#'  \code{\link{Lrnr_base}}.
#'
#' @family Learners
#'
#' @section Parameters:
#'   - \code{grid_type = "equal_range"}: A \code{character} indicating the
#'       strategy to be used in creating bins along the observed support of
#'       \code{A}. For bins of equal range, use \code{"equal_range"}; consult
#'       the documentation of \code{\link[ggplot2]{cut_interval}} for further
#'       information. To ensure each bin has the same number of observations,
#'       use \code{"equal_mass"}; consult the documentation of
#'       \code{\link[ggplot2]{cut_number}} for details. The default is
#'       \code{"equal_range"} since this has been found to provide better
#'       performance in simulation experiments; however, both types may be
#'       specified (i.e., \code{c("equal_range", "equal_mass")}) together, in
#'       which case cross-validation will be used to select the optimal binning
#'       strategy.
#'   - \code{n_bins = c(3, 5, 10)}: This \code{numeric} value indicates the
#'       number(s) of bins into which the support of \code{A} is to be divided.
#'       As with \code{grid_type}, multiple values may be specified, in which
#'       cross-validation will be used to select the optimal number of bins.
#'   - \code{lambda_seq = exp(seq(-1, -13, length = 1000L))}: A \code{numeric}
#'       sequence of regularization parameter values of Lasso regression, which
#'       are passed to \code{\link[hal9001]{fit_hal}} via its argument
#'       \code{lambda}, itself passed to \code{\link[glmnet]{glmnet}}.
#'   - \code{...}: Other arguments to be passed directly to
#'       \code{\link[haldensify]{haldensify}}. See its documentation for
#'       details.
#'
#' @examples
#' data(cpp_imputed)
#' covars <- c("parity", "sexn")
#' outcome <- "haz"
#'
#' # create task
#' task <- cpp_imputed %>%
#'   dplyr::slice(seq(1, nrow(.), by = 3)) %>%
#'   dplyr::filter(agedays == 1) %>%
#'   sl3_Task$new(
#'     covariates = covars,
#'     outcome = outcome
#'   )
#'
#' # instantiate the learner
#' hal_dens <- Lrnr_haldensify$new(
#'   grid_type = "equal_range",
#'   n_bins = c(3, 5),
#'   lambda_seq = exp(seq(-1, -13, length = 100))
#' )
#'
#' # fit and predict densities
#' hal_dens_fit <- hal_dens$train(task)
#' hal_dens_preds <- hal_dens_fit$predict()
Lrnr_haldensify <- R6Class(
  classname = "Lrnr_haldensify", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(grid_type = "equal_range",
                          n_bins = c(3, 5, 10),
                          lambda_seq = exp(seq(-1, -13, length = 1000L)),
                          ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  active = list(
    name = function() {
      if (is.null(private$.name)) {
        params <- self$params
        if (length(params) > 0) {
          atom_params <- sapply(params, is.atomic)
          params <- params[atom_params]
        }
        props <- c(list(class(self)[1]), params)
        props$lambda_seq <- NULL
        name <- paste(props, collapse = "_")
        private$.name <- name
      }
      return(private$.name)
    }
  ),
  private = list(
    .properties = c("density"),

    .train = function(task) {
      args <- self$params

      outcome_type <- self$get_outcome_type(task)

      if (is.null(args$family)) {
        args$family <- outcome_type$glm_family(return_object = TRUE)$family
      }

      args$W <- as.matrix(task$X)
      args$A <- as.numeric(outcome_type$format(task$Y))
      args$use_future <- FALSE

      if (task$has_node("weights")) {
        args$wts <- task$weights
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset
      }

      fit_object <- call_with_args(haldensify::haldensify, args)
      return(fit_object)
    },
    .predict = function(task = NULL) {
      predictions <- predict(self$fit_object,
        new_A = as.numeric(task$Y),
        new_W = as.matrix(task$X)
      )
      return(predictions)
    },
    .required_packages = c("haldensify")
  )
)
