#' The Scalable Highly Adaptive Lasso
#'
#' The Highly Adaptive Lasso (HAL) is a nonparametric regression function that
#' has been demonstrated to optimally estimate functions with bounded (finite)
#' variation norm. The algorithm proceeds by first building an adaptive basis
#' (i.e., the HAL basis) based on indicator basis functions (or higher-order
#' spline basis functions) representing covariates and interactions of the
#' covariates up to a pre-specified degree. The fitting procedures included in
#' this learner use \code{\link[hal9001]{fit_hal}} from the \pkg{hal9001}
#' package. For details on HAL regression, consider consulting the following
#' \insertCite{benkeser2016hal;textual}{sl3}),
#' \insertCite{coyle2020hal9001-rpkg;textual}{sl3}),
#' \insertCite{hejazi2020hal9001-joss;textual}{sl3}).
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stats predict quasibinomial
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
#'   - \code{max_degree=3}: The highest order of interaction terms for which
#'       the basis functions ought to be generated. The default corresponds to
#'       generating basis functions up to all 3-way interactions of covariates
#'       in the input matrix, matching the default in \pkg{hal9001}.
#'   - \code{fit_type="glmnet"}: The specific routine to be called when fitting
#'       the Lasso regression in a cross-validated manner. Choosing the
#'       \code{"glmnet"} option calls either \code{\link[glmnet]{cv.glmnet}} or
#'       \code{\link[glmnet]{glmnet}}.
#'   - \code{n_folds=10}: Integer for the number of folds to be used when
#'       splitting the data for cross-validation. This defaults to 10 as this
#'       is the convention for V-fold cross-validation.
#'   - \code{use_min=TRUE}: Determines which lambda is selected from
#'       \code{\link[glmnet]{cv.glmnet}}. \code{TRUE} corresponds to
#'       \code{"lambda.min"} and \code{FALSE} corresponds to
#'       \code{"lambda.1se"}.
#'   - \code{reduce_basis=NULL}: A \code{numeric} value bounded in the open
#'       interval (0,1) indicating the minimum proportion of ones in a basis
#'       function column needed for the basis function to be included in the
#'       fitting the HAL model. Any basis functions with a lower proportion of
#'       1's than the specified cutoff will be removed. This argument defaults
#'       to \code{NULL}, in which case all basis functions are used in fitting.
#'   - \code{return_lasso=TRUE}: A \code{logical} indicating whether or not to
#'       return the \code{\link[glmnet]{glmnet}} fit of the HAL model.
#'   - \code{return_x_basis=FALSE}: A \code{logical} indicating whether or not
#'       to return the matrix of (possibly reduced) basis functions used in the
#'       HAL fit.
#'   - \code{basis_list=NULL}: The full set of basis functions generated from
#'       the input data (from \code{\link[hal9001]{enumerate_basis}}). The
#'       dimensionality of this structure is roughly (n * 2^(d - 1)), where n
#'       is the number of observations and d is the number of columns.
#'   - \code{cv_select=TRUE}: A \code{logical} specifying whether the array of
#'       values specified should be passed to \code{\link[glmnet]{cv.glmnet}}
#'       in order to pick the optimal value (based on cross-validation) (when
#'       set to \code{TRUE}) or to fit along the sequence of values (or single
#'       value using \code{\link[glmnet]{glmnet}} (when set to \code{FALSE}).
#'   - \code{...}: Other parameters passed to \code{\link[hal9001]{fit_hal}}.
#'       See its documentation for details.
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' data(mtcars)
#' mtcars_task <- sl3_Task$new(
#'   data = mtcars,
#'   covariates = c(
#'     "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
#'     "gear", "carb"
#'   ),
#'   outcome = "mpg"
#' )
#' # simple prediction with HAL
#' hal_lrnr <- Lrnr_hal9001$new()
#' hal_fit <- hal_lrnr$train(mtcars_task)
#' hal_preds <- hal_fit$predict()
Lrnr_hal9001 <- R6Class(
  classname = "Lrnr_hal9001", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(max_degree = 3,
                          fit_type = "glmnet",
                          n_folds = 10,
                          use_min = TRUE,
                          reduce_basis = NULL,
                          return_lasso = TRUE,
                          return_x_basis = FALSE,
                          basis_list = NULL,
                          cv_select = TRUE,
                          ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial", "weights", "ids"),

    .train = function(task) {
      args <- self$params

      outcome_type <- self$get_outcome_type(task)

      if (is.null(args$family)) {
        args$family <- outcome_type$glm_family()
      }

      if (args$family %in% "quasibinomial") {
        args$family <- stats::quasibinomial()
      }

      args$X <- as.matrix(task$X)
      args$Y <- outcome_type$format(task$Y)
      args$yolo <- FALSE

      if (task$has_node("weights")) {
        args$weights <- task$weights
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset
      }

      if (task$has_node("id")) {
        args$id <- task$id
      }

      # pass in formals of glmnet versus cv.glmnet based on cv_select
      if (args$cv_select) {
        glmnet_formals <- union(
          names(formals(glmnet::cv.glmnet)),
          names(formals(glmnet::glmnet))
        )
      } else {
        glmnet_formals <- names(formals(glmnet::glmnet))
      }
      glmnet_formals <- setdiff(glmnet_formals, c("x", "y"))

      # fit HAL, allowing formal glmnet and cv.glmnet arguments
      fit_object <- call_with_args(
        hal9001::fit_hal, args,
        other_valid = glmnet_formals
      )
      return(fit_object)
    },
    .predict = function(task = NULL) {
      predictions <- stats::predict(
        self$fit_object,
        new_data = as.matrix(task$X)
      )
      if (!is.na(safe_dim(predictions)[2])) {
        p <- ncol(predictions)
        colnames(predictions) <- sprintf("lambda_%0.3e", self$params$lambda)
      }
      return(predictions)
    },
    .required_packages = c("hal9001", "glmnet")
  )
)
