#' GLMs with Elastic Net Regularization
#'
#' This learner provides fitting procedures for elastic net models, including
#' both lasso (L1) and ridge (L2) penalized regression, using the \pkg{glmnet}
#' package. The function \code{\link[glmnet]{cv.glmnet}} is used to select an
#' appropriate value of the regularization parameter lambda. For details on
#' these regularized regression models and \pkg{glmnet}, consider consulting
#' \insertCite{glmnet;textual}{sl3}).
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stats predict
#' @importFrom assertthat assert_that is.count is.flag
#' @importFrom origami folds2foldvec make_folds
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
#'  - \code{lambda = NULL}: An optional vector of lambda values to compare.
#'  - \code{type.measure = "deviance"}: The loss to use when selecting
#'      lambda. Options documented in \code{\link[glmnet]{cv.glmnet}}.
#'  - \code{nfolds = 10}: Number of folds to use for internal cross-validation.
#'  - \code{alpha = 1}: The elastic net parameter: \code{alpha = 0} is Ridge
#'      (L2-penalized) regression, while \code{alpha = 1} specifies Lasso
#'      (L1-penalized) regression. Values in the closed unit interval specify a
#'      weighted combination of the two penalties. For further details, consult
#'      the documentation of \code{\link[glmnet]{glmnet}}.
#'  - \code{nlambda = 100}: The number of lambda values to fit. Comparing
#'      fewer values will speed up computation, but may hurt the statistical
#'      performance. For further details, consult the documentation of
#'      \code{\link[glmnet]{cv.glmnet}}.
#'  - \code{use_min = TRUE}: If \code{TRUE}, the smallest value of the lambda
#'      regularization parameter is used for prediction (i.e.,
#'      \code{lambda = cv_fit$lambda.min}); otherwise, a larger value is used
#'      (i.e., \code{lambda = cv_fit$lambda.1se}). The distinction between the
#'      two variants is clarified in the documentation of
#'      \code{\link[glmnet]{cv.glmnet}}.
#'  - \code{stratify_cv = FALSE}: Stratify internal cross-validation folds, so
#'      that a binary outcome's prevalence for training is roughly the same in
#'      the training and validation sets of the internal cross-validation
#'      folds? This argument can only be used when the outcome type for
#'      training is binomial; and either the \code{id} node in the task is not
#'      specified, or \code{\link[glmnet]{cv.glmnet}}'s \code{foldid} argument
#'      is not specified upon initializing the learner.
#'  - \code{...}: Other parameters passed to \code{\link[glmnet]{cv.glmnet}}
#'      and \code{\link[glmnet]{glmnet}}.
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
#' # simple prediction with lasso penalty
#' lasso_lrnr <- Lrnr_glmnet$new()
#' lasso_fit <- lasso_lrnr$train(mtcars_task)
#' lasso_preds <- lasso_fit$predict()
#'
#' # simple prediction with ridge penalty
#' ridge_lrnr <- Lrnr_glmnet$new(alpha = 0)
#' ridge_fit <- ridge_lrnr$train(mtcars_task)
#' ridge_preds <- ridge_fit$predict()
Lrnr_glmnet <- R6Class(
  classname = "Lrnr_glmnet",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(lambda = NULL, type.measure = "deviance",
                          nfolds = 10, alpha = 1, nlambda = 100,
                          use_min = TRUE, stratify_cv = FALSE, ...) {
      super$initialize(params = args_to_list(), ...)
    }
  ),

  private = list(
    .properties = c(
      "continuous", "binomial", "categorical",
      "weights", "ids"
    ),

    .train = function(task) {
      args <- self$params

      outcome_type <- self$get_outcome_type(task)

      if (is.null(args$family)) {
        args$family <- outcome_type$glm_family()
      }

      if (args$family %in% "quasibinomial") {
        args$family <- stats::quasibinomial()
      }

      # specify data
      args$x <- as.matrix(task$X)
      args$y <- outcome_type$format(task$Y)

      if (task$has_node("weights")) {
        args$weights <- task$weights
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset
      }

      if (task$has_node("id")) {
        args$foldid <- origami::folds2foldvec(task$folds)
      }

      if (args$stratify_cv) {
        if (outcome_type$type == "binomial" & is.null(args$foldid)) {
          folds <- origami::make_folds(
            n = length(args$y), strata_ids = args$y, fold_fun = folds_vfold, 
            V = as.integer(args$nfolds)
          )
          args$foldid <- origami::folds2foldvec(folds)
        } else {
          warning(
            "stratify_cv is TRUE; but inner cross-validation folds cannot ",
            "be stratified. Either the outcome is not binomial, or foldid ",
            "has already been established (user specified foldid upon ",
            "initializing the learner, or it was set according to task id's)."
          )
        }
      }

      fit_object <- call_with_args(
        glmnet::cv.glmnet, args,
        other_valid = names(formals(glmnet::glmnet)),
        ignore = c("use_min", "stratify_cv")
      )
      fit_object$glmnet.fit$call <- NULL
      return(fit_object)
    },

    .predict = function(task) {
      outcome_type <- private$.training_outcome_type

      # set choice regularization penalty
      if (self$params$use_min) {
        lambda <- "lambda.min"
      } else {
        lambda <- "lambda.1se"
      }

      # get predictions via S3 method
      predictions <- stats::predict(
        private$.fit_object,
        newx = as.matrix(task$X), type = "response",
        s = lambda
      )

      # reformat predictions based on outcome type
      if (outcome_type$type == "categorical") {
        cat_names <- dimnames(predictions)[[2]]
        # predictions is a 3-dim matrix, convert to 2-dim matrix
        dim(predictions) <- dim(predictions)[1:2]
        colnames(predictions) <- cat_names
        # pack predictions in a single column
        predictions <- pack_predictions(predictions)
      }
      return(predictions)
    },
    .required_packages = c("glmnet", "origami")
  )
)
