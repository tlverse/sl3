#' Conditional Density Estimation with the Highly Adaptive LASSO
#'
#' @docType class
#' @importFrom R6 R6Class
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
#'   \item{\code{grid_type = c("equal_range", "equal_mass")}}{\code{character}
#'    indicating the strategy to be used in creating bins along the observed
#'    support of the outcome variable. For bins of equal range, use
#'    "equal_range" (based on \code{\link[ggplot2]{cut_interval}}). To ensure
#'    each bin has the same number of observations, use "equal_mass" (based on
#'    \code{\link[ggplot2]{cut_number}}).
#'   }
#'   \item{\code{n_bins = c(3, 5)}}{Only used if \code{type} is set to
#'    \code{"equal_range"} or \code{"equal_mass"}. This \code{numeric} value
#'    indicates the number of bins that the support of the outcome variable is
#'    to be divided into.
#'   }
#'   \item{\code{lambda_seq = exp(seq(-1, -13, length = 1000L))}}{\code{numeric}
#'    sequence of values of the regulariztion parameter of the Lasso regression,
#'    to be passed to to \code{\link[hal9001]{fit_hal}}.
#'   }
#'   \item{\code{trim_dens = 1/sqrt(n)}}{A \code{numeric} giving the minimum
#'     allowed value of the resultant density predictions. Any predicted
#'     density values below this tolerance threshold are set to the indicated
#'     minimum. The default is to use the inverse of the square root of the
#'     sample size of the prediction set, i.e., 1/sqrt(n); another notable
#'     choice is 1/sqrt(n)/log(n). If there are observations in the prediction
#'     set with values of \code{new_A} outside of the support of the training
#'     set, their predictions are similarly truncated.
#'   }
#'   \item{\code{...}}{ Other parameters passed directly to
#'    \code{\link[haldensify]{haldensify}}. See its documentation for details.
#'   }
#' }
#
Lrnr_haldensify <- R6Class(
  classname = "Lrnr_haldensify", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(grid_type = "equal_range",
                          n_bins = c(3, 5),
                          lambda_seq = exp(seq(-1, -13, length = 1000L)),
                          trim_dens = NULL,
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

      # extract input data
      args$W <- as.matrix(task$X)
      args$A <- as.numeric(outcome_type$format(task$Y))

      # handle weights
      if (task$has_node("weights")) {
        args$wts <- task$weights
      }

      # extract offset
      if (task$has_node("offset")) {
        args$offset <- task$offset
      }

      # fit haldensify conditional density estimator
      fit_object <- call_with_args(
        haldensify::haldensify, args,
        other_valid = c(
          "max_degree", "smoothness_orders", "num_knots", "reduce_basis",
          "fit_control"
        ),
        ignore = c("cv_select", "weights", "family", "fit_type", "trim_dens")
      )
      return(fit_object)
    },
    .predict = function(task = NULL) {
      # set density trimming to haldensify::predict default if NULL
      if (is.null(self$params[["trim_dens"]])) {
        trim_dens <- 1 / sqrt(task$nrow)
      } else {
        trim_dens <- self$params[["trim_dens"]]
      }

      # predict density
      predictions <- predict(self$fit_object,
        new_A = as.numeric(task$Y),
        new_W = as.matrix(task$X),
        trim_dens = trim_dens
      )
      return(predictions)
    },
    .required_packages = c("haldensify")
  )
)
