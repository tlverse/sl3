#' GAM: Generalized Additive Models
#'
#' This learner provides fitting procedures for generalized additive models,
#' using the routines from \pkg{mgcv} through a call to the function
#' \code{\link[mgcv]{gam}}. The \pkg{mgcv} package and the use of GAMs are
#' described thoroughly (with examples) in \insertCite{mgcv;textual}{sl3},
#' while \insertCite{hastie-gams;textual}{sl3} also provided an earlier quite
#' thorough look at GAMs.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stats predict family
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
#'   - \code{formula}: An optional argument specifying the formula of GAM.
#'       Input type can be formula or string, or a list of them. If not
#'       specified, continuous covariates will be smoothened with the smooth
#'       terms represented using "penalized thin plate regression splines". For
#'       a more detailed description, please consult the documentation for
#'       \code{\link[mgcv]{gam}}.
#'   - \code{family}: An optional argument specifying the family of the GAM.
#'       See \code{\link[stats]{family}} and \code{\link[mgcv]{family.mgcv}}
#'       for a list of available family functions. If left unspecified, it will
#'       be inferred depending on the detected type of the outcome. For now,
#'       GAM supports binomial and gaussian outcome types, if \code{formula} is
#'       unspecified. For a more detailed description of this argument, please
#'       consult the documentation of \code{\link[mgcv]{gam}}.
#'   - \code{method}: An optional argument specifying the method for smoothing
#'       parameter selection. The default is global cross-validation (GCV). For
#'       more detaileds on this argument, consult the documentation of
#'       \code{\link[mgcv]{gam}}.
#'   - \code{...}: Other parameters passed to \code{\link[mgcv]{gam}}. See its
#'       documentation for details.
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' data(cpp_imputed)
#' # create task for prediction
#' cpp_task <- sl3_Task$new(
#'   data = cpp_imputed,
#'   covariates = c("bmi", "parity", "mage", "sexn"),
#'   outcome = "haz"
#' )
#' # initialization, training, and prediction with the defaults
#' gam_lrnr <- Lrnr_gam$new()
#' gam_fit <- gam_lrnr$train(cpp_task)
#' gam_preds <- gam_fit$predict()
Lrnr_gam <- R6Class(
  classname = "Lrnr_gam", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(formula = NULL,
                          family = NULL,
                          method = "GCV.Cp",
                          ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial"),
    .train = function(task) {
      # load args
      args <- self$params
      # process args
      ## data
      outcome_type <- self$get_outcome_type(task)
      Y <- data.frame(outcome_type$format(task$Y))
      colnames(Y) <- task$nodes$outcome
      args$data <- cbind(task$X, Y)
      ## family
      if (is.null(args$family)) {
        if (outcome_type$type == "continuous") {
          args$family <- stats::gaussian()
        } else if (outcome_type$type == "binomial") {
          args$family <- stas::binomial()
        } else if (outcome_type$type == "categorical") {
          # TODO: implement categorical?
          # NOTE: must specify (#{categories}-1)+linear_predictors) in formula
          stop("Categorical outcomes are unsupported by Lrnr_gam for now.")
        } else {
          stop("Specified outcome type is unsupported by Lrnr_gam.")
        }
      }
      ## formula
      if (is.null(args$formula)) {
        covars_type <- lapply(
          task$X,
          function(iter) variable_type(x = iter)$type
        )
        i_discrete <- covars_type %in% c("binomial", "categorical")
        X_discrete <- task$X[, ..i_discrete]
        i_continuous <- covars_type == "continuous"
        X_continuous <- task$X[, ..i_continuous]
        "y ~ s(x1) + s(x2) + x3"
        X_smooth <- sapply(colnames(X_continuous), function(x) {
          unique_x <- unlist(unique(task$X[, x, with = F]))
          if (length(unique_x) < 10) {
            paste0("s(", x, ", k=", length(unique_x), ")")
          } else {
            paste0("s(", x, ")")
          }
        })
        if (length(X_continuous) > 0 & length(X_discrete) > 0) {
          args$formula <- as.formula(paste(c(
            colnames(Y),
            paste(c(
              X_smooth,
              colnames(X_discrete)
            ),
            collapse = " + "
            )
          ),
          collapse = " ~ "
          ))
        } else if (length(X_continuous) > 0) {
          args$formula <- as.formula(paste(c(
            colnames(Y),
            paste(X_smooth,
              collapse = " + "
            )
          ),
          collapse = " ~ "
          ))
        } else if (length(X_discrete) > 0) {
          args$formula <- as.formula(paste(c(
            colnames(Y),
            paste(colnames(X_discrete),
              collapse = " + "
            )
          ),
          collapse = " ~ "
          ))
        } else {
          stop("Specified covariates types are unsupported in Lrnr_gam.")
        }
      } else if (is.list(args$formula)) {
        args$formula <- lapply(args$formula, as.formula)
      } else {
        args$formula <- as.formula(args$formula)
      }

      # fit
      fit_object <- call_with_args(mgcv::gam, args)
      return(fit_object)
    },
    .predict = function(task) {
      # get predictions
      predictions <- stats::predict(
        private$.fit_object,
        newdata = task$X,
        type = "response"
      )
      predictions <- as.numeric(predictions)
      return(predictions)
    },
    .required_packages = c("mgcv")
  )
)
