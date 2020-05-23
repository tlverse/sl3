#' RFCDE: Random Forests for Conditional Density Estimation
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
#'   \item{\code{n_trees = 1000}}{ The number of trees in the forest. Defaults
#'    to 1000.
#'   }
#'   \item{\code{node_size = 5}}{ The minimum number of observations in a leaf
#'    node. Defaults to 5.
#'   }
#'   \item{\code{n_basis}}{ The number of basis functions used in split density
#'    estimates. Defaults to 31.
#'   }
#'   \item{\code{basis_system}}{ The system of basis functions to use; currently
#'    "cosine" and "Haar" are supported. Defaults to "cosine".
#'   }
#'   \item{\code{min_loss_delta}}{ The minimum loss for a split. Defaults to
#'    0.0.
#'   }
#'   \item{\code{fit_oob}}{ Whether to fit out-of-bag samples or not. Out-of-bag
#'    samples increase the computation time but allows for estimation of the
#'    prediction loss. Defaults to \code{FALSE.}
#'   }
#'   \item{\code{z_grid}}{ Grid points at which to evaluate the kernel density.
#'    The default value is given as an example but should be customized to make
#'    the prediction procedure suitable for a given use case.
#'   }
#'   \item{\code{bandwidth}}{ Bandwidth for kernel density estimates (optional).
#'    Defaults to \code{"auto"} for automatic bandwidth selection.
#'   }
#'   \item{\code{output_type}}{ Whether to return the density evaluated over a
#'    grid of supplied points (using \code{z_grid}) or at the observed outcome
#'    values. Default is to return the density evaluated at only the observed
#'    outcome values (option \code{"observed"}); choose \code{"grid"} to return
#'    instead the density evaluated over the arbitrary input grid.
#'   }
#'   \item{\code{...}}{ Other parameters passed directly to \code{RFCDE}.
#'    Consult the documentation of that package for details.
#'   }
#' }
#
Lrnr_rfcde <- R6Class(
  classname = "Lrnr_rfcde", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(n_trees = 1000,
                          node_size = 5,
                          n_basis = 31,
                          basis_system = "cosine",
                          min_loss_delta = 0.0,
                          fit_oob = FALSE,
                          z_grid = seq(0, 10, length.out = 100),
                          bandwidth = "auto",
                          output_type = "observed",
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
          atom_params["z_grid"] <- FALSE
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

      args$x_train <- as.matrix(task$X)
      args$z_train <- as.numeric(outcome_type$format(task$Y))

      if (task$has_node("weights")) {
        warning("Lrnr_rfcde and RFCDE do not appear to support weights.")
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset
      }

      fit_object <- call_with_args(RFCDE::RFCDE, args)
      return(fit_object)
    },
    .predict = function(task = NULL) {
      # extra arguments required for prediction method
      bandwidth <- self$params$bandwidth
      n_grid <- self$params$n_grid
      z_grid <- self$params$z_grid
      output_type <- self$params$output_type

      if (output_type == "grid") {
        predictions <- predict(self$fit_object,
          newdata = as.matrix(task$X),
          response = "CDE",
          z_grid = z_grid,
          bandwidth = bandwidth
        )
        return(predictions)
      } else if (output_type == "observed") {
        density_pred <- lapply(seq_along(task$Y), function(idx) {
          density_pred_idx <- predict(self$fit_object,
            newdata = as.matrix(task$X[idx, ]),
            response = "CDE",
            z_grid = as.numeric(task$Y[idx]),
            bandwidth = bandwidth
          )
          return(density_pred_idx)
        })
        predictions <- do.call(c, density_pred)
        predictions <- unname(predictions)
        return(predictions)
      }
    },
    .required_packages = c("RFCDE")
  )
)
