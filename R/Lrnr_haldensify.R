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
#'   \item{\code{grid_type = "grid_type"}}{A \code{character} indicating the
#'    strategy to be used in creating bins along the observed support of the
#'    outcome variable. For bins of equal range, use "equal_range", consulting
#'    the documentation of \code{ggplot2::cut_interval} for more information. To
#'    ensure each bins has the same number of points, use "equal_mass" and
#'    consult the documentation of \code{ggplot2::cut_number} for details.
#'   }
#'   \item{\code{n_bins = "n_bins"}}{ Only used if \code{type} is set to
#'    \code{"equal_range"} or \code{"equal_mass"}. This \code{numeric} value
#'    indicates the number of bins that the support of the outcome variable is
#'    to be divided into.
#'   }
#'   \item{\code{lambda_seq = "lambda_seq"}}{ A \code{numeric} sequence of
#'    values of the lambda tuning parameter of the Lasso L1 regression, to be
#'    passed to \code{glmnet::glmnet} through a call to \code{hal9001::fit_hal}.
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
    initialize = function(grid_type = c("equal_range", "equal_mass"),
                          n_bins = c(5, 10),
                          lambda_seq = exp(seq(-1, -13, length = 1000)),
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
