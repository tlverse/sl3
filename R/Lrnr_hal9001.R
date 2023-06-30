#' Scalable Highly Adaptive Lasso (HAL)
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
#' @importFrom origami folds2foldvec
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
#'   - \code{max_degree = 2}: An integer specifying the highest order of
#'       interaction terms for which basis functions ought to be generated.
#'   - \code{smoothness_orders = 1}: An integer specifying the smoothness of
#'       the basis functions. See details of \code{hal9001} package's
#'       \code{\link[hal9001]{fit_hal}} function for more information.
#'   - \code{num_knots = 5}: An integer vector of length 1 or of length
#'       \code{max_degree}, specifying the maximum number of knot points
#'       (i.e., bins) for each covariate. If \code{num_knots} is a unit-length
#'       vector, then the same \code{num_knots} are used for each degree. See
#'       details of \code{hal9001} package's \code{\link[hal9001]{fit_hal}}
#'       function for more information.
#'   - \code{fit_control}: List of arguments, including those specified in
#'      \code{\link[hal9001]{fit_hal}}'s  \code{fit_control} documentation, and
#'      any additional arguments to be passed to \code{\link[glmnet]{cv.glmnet}}
#'      or \code{\link[glmnet]{glmnet}}. See the \code{hal9001} package
#'      \code{\link[hal9001]{fit_hal}} function fdocumentation or more
#'      information.
#'   - \code{...}: Other parameters passed to \code{\link[hal9001]{fit_hal}}
#'       and additional arguments defined in \code{\link{Lrnr_base}}, such as
#'       \code{params} like \code{formula}.
#'
#' @examples
#' data(cpp_imputed)
#' covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
#' task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")
#'
#' # instantiate with max 2-way interactions, 0-order splines, and binning
#' # (i.e., num_knots) that decreases with increasing interaction degree
#' hal_lrnr <- Lrnr_hal9001$new(max_degree = 2, num_knots = c(5, 3))
#' hal_fit <- hal_lrnr$train(task)
#' hal_preds <- hal_fit$predict()
Lrnr_hal9001 <- R6Class(
  classname = "Lrnr_hal9001",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(max_degree = 2,
                          smoothness_orders = 1,
                          num_knots = 5,
                          fit_control = list(nfolds = 10),
                          ...) {
      if (!is.null(fit_control) & !is.list(fit_control)) {
        stop("fit_control must be specified as a list of arguments")
      }
      params <- args_to_list()
      super$initialize(params = params, ...)
    },
    process_formula = function(task) {
      return(task)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial", "weights", "ids", "cv"),
    .train = function(task) {
      args <- self$params

      verbose <- args$verbose
      if (is.null(verbose)) {
        verbose <- getOption("sl3.verbose")
      }

      args$X <- as.matrix(task$X)

      outcome_type <- self$get_outcome_type(task)
      args$Y <- outcome_type$format(task$Y)

      if (is.null(args$family)) {
        args$family <- outcome_type$glm_family()
      }

      # instantiate fit_control if it's not specified
      if (!any(grepl("fit_control", names(args)))) {
        args$fit_control <- list()
      }

      # if fit_control$cv_select is NULL or TRUE, then HAL will use
      # glmnet::cv.glmnet(), and a specific CV scheme will be passed via
      # fit_control$foldid to fit_hal
      if ((is.null(args$fit_control$cv_select) || args$fit_control$cv_select) &
        is.null(args$fit_control$foldid)) {
        hal_nfolds <- args$fit_control$nfolds # number of CV folds for HAL
        folds <- task$folds # training task's CV folds

        # we need to create the folds when hal_nfolds is provided and it is not
        # equal to the number of folds in the task, otherwise we
        # can just use "folds" (above) as the CV folds for fitting HAL
        if (!is.null(hal_nfolds) && length(folds) != hal_nfolds) {
          folds <- task$get_folds(V = hal_nfolds)
        }
        args$fit_control$foldid <- origami::folds2foldvec(folds)
      }

      if (task$has_node("weights")) {
        if (packageVersion("hal9001") >= "0.4.5") {
          args$weights <- task$weights
        } else {
          args$fit_control$weights <- task$weights
        }
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset
      }

      fit_object <- call_with_args(hal9001::fit_hal, args)
      return(fit_object)
    },
    .predict = function(task = NULL) {
      predictions <- stats::predict(
        self$fit_object,
        new_data = data.matrix(task$X)
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
