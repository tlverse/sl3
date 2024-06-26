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
#'  - \code{nfolds = 10}: Number of k-fold/V-fold cross-validation folds for
#'      \code{cv.glmnet} to consider when selecting the optimal \code{lambda}
#'      with cross-validation. Smallest {nfolds} value allowed by \code{glmnet}
#'      is 3. For further details, consult the documentation of
#'      \code{\link[glmnet]{cv.glmnet}}.
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
#'  - \code{nfolds = 10}: Number of folds (default is 10). Smallest value
#'      allowable by \code{glmnet} is 3.
#'  - \code{...}: Other parameters passed to \code{\link[glmnet]{cv.glmnet}}
#'      and \code{\link[glmnet]{glmnet}}, and additional arguments defined in
#'      \code{\link{Lrnr_base}}, such as \code{params} like \code{formula}.
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
                          use_min = TRUE, ...) {
      super$initialize(params = args_to_list(), ...)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial", "categorical", "weights", "ids", "cv"),
    .train = function(task) {
      args <- self$params

      verbose <- args$verbose
      if (is.null(verbose)) {
        verbose <- getOption("sl3.verbose")
      }

      outcome_type <- self$get_outcome_type(task)

      if (is.null(args$family)) {
        args$family <- outcome_type$glm_family()
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

      # specify internal CV via foldid
      if (is.null(args$foldid)) {
        folds <- task$folds
        # we need to create the folds when args$nfolds is provided and it is
        # not equal to the number of folds in the task, otherwise we
        # can just use "folds" (above) as the CV folds for fitting cv.glmnet
        if (!is.null(args$nfolds) && length(folds) != args$nfolds) {
          folds <- task$get_folds(V = args$nfolds)
        }
        args$foldid <- origami::folds2foldvec(folds)
      }

      fit_object <- call_with_args(
        glmnet::cv.glmnet, args,
        other_valid = names(formals(glmnet::glmnet)),
        ignore = "use_min"
      )
      fit_object$glmnet.fit$call <- NULL
      return(fit_object)
    },
    .predict = function(task) {
      args <- list(
        object = private$.fit_object, newx = as.matrix(task$X), type = "response"
      )

      # set choice regularization penalty
      if (self$params$use_min) {
        args$s <- "lambda.min"
      } else {
        args$s <- "lambda.1se"
      }

      if (task$has_node("offset")) {
        if (private$.fit_object$glmnet.fit$offset) {
          args$newoffset <- task$offset
        } else {
          warning(
            "Prediction task has offset, but an offset was not included in ",
            "the task for training the glmnet learner. The prediction task's ",
            "offset will not be considered for prediction."
          )
        }
      }

      # get predictions via S3 method
      predictions <- do.call(stats::predict, args)

      # reformat predictions based on outcome type
      if (private$.training_outcome_type$type == "categorical") {
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
