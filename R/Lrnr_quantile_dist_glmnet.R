#' GLMs with Elastic Net Regularization and Quantile Distance Interactions
#'
#' This learner provides fitting procedures for elastic net models, including
#' both lasso (L1) and ridge (L2) penalized regression, using the \pkg{glmnet}
#' package. The function \code{\link[glmnet]{cv.glmnet}} is used to select an
#' appropriate value of the regularization parameter lambda. For details on
#' these regularized regression models and \pkg{glmnet}, consider consulting
#' \insertCite{glmnet;textual}{sl3}). This learner also searches higher-order 
#' interactions by including interactions with the highest and lowest quantile distance
#' ranking. 
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
#'  - \code{nfolds = 10}: Number of folds to use for internal cross-validation.
#'  - \code{n_quantiles = 10}: Number of quantiles to bin numeric data
#'  - \code{degree = 2}: Degree of interactions to search
#'  - \code{q_tails = 0.05}: Percent by which to select variables with distances on the upper and lower tails
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
Lrnr_quantile_dist_glmnet <- R6Class(
  classname = "Lrnr_quantile_dist_glmnet",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(lambda = NULL, n_quantiles = 10, degree = 2, q_tails = 0.05, type.measure = "deviance",
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
      
      X_levels <- sapply(task$X, function(x) length(unique(x)))
      X_discrete <- task$X[, X_levels <= 2, with=F]
      X_continuous <- task$X[, X_levels > 2, with=F]
      breaks_list <- lapply(seq_along(X_continuous), function(i){
        breaks = unique(c(min(X_continuous[[i]]), as.numeric(quantile(X_continuous[[i]], probs = seq(100/(n_quantiles+1), 100-(100/(n_quantiles+1)), by = 100/(n_quantiles+1))/100)), max(X_continuous[[i]])))
      })
      names(breaks_list) <- names(X_continuous)
      
      X_continuous <- as.data.table(do.call(cbind, lapply(seq_along(breaks_list), function(i){
        name_x <- names(breaks_list)[i]
        cut(task$X[[name_x]], breaks = breaks_list[[i]], labels = FALSE, include.lowest = TRUE)
      })))
      names(X_continuous) <- names(breaks_list)
      X <- cbind(X_continuous, X_discrete)
      X <- X[,names(task$X),with=F]
      
      X[,(colnames(X)):= lapply(.SD, as.factor), .SDcols = colnames(X)]
      levels_list <- sapply(X, levels)
      # levels_list <- levels_list[names(levels_list) %in% names(breaks_list)]
      
      quantile_dist <- as.matrix(dist(t(X)))
      
      quantile_dist_flat <- data.frame(rows=rownames(quantile_dist)[row(quantile_dist)], vars=colnames(quantile_dist)[col(quantile_dist)], values=c(quantile_dist))
      quantile_dist_flat <- quantile_dist_flat[order(quantile_dist_flat$values),]
      quantile_dist_flat <- quantile_dist_flat[quantile_dist_flat$rows != quantile_dist_flat$vars,]
      quantile_dist_flat <- quantile_dist_flat[duplicated(quantile_dist_flat$values), ]
      
      tails <- quantile(quantile_dist_flat$values, probs = c(0.05, 1- 0.05))
      positive_assocs <- quantile_dist_flat[quantile_dist_flat$values <= tails[1],]
      negative_assocs <- quantile_dist_flat[quantile_dist_flat$values >= tails[2],]

      intxns <- rbind(positive_assocs, negative_assocs)
      
      additive <- paste(colnames(X), collapse = "+")
      multiplicative <- paste(paste(intxns$rows, intxns$vars, sep = "*"), collapse = " + ")
      formula <- as.formula(paste("~", paste(additive, multiplicative, sep = " + "), "-1", sep = ""))
      
      train_df_with_ints <- as.data.frame(
        model.matrix(
          data = X, 
          object = formula))
      
      args$x <- as.matrix(df_with_ints)
      args$y <- outcome_type$format(task$Y)
      
      # task$basis <- args$x
      
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
      
      non_glmnet_args <- c("quantiles", "degree", "n_intxn_tails")
      args <- args[args != non_glmnet_args]
      
      fit_object <- list()
      
      fit_object$fit_object <- call_with_args(
        glmnet::cv.glmnet, args,
        other_valid = names(formals(glmnet::glmnet)),
        ignore = c("use_min", "stratify_cv")
      )
      fit_object$basis_formula <- formula
      fit_object$breaks_list <- breaks_list
      fit_object$levels_list <- levels_list
      fit_object$basis_names <- colnames(train_df_with_ints)
      # fit_object$glmnet.fit$data <- args$x
      # results <- list("fit_object" = fit_object, "data" =  as.matrix(df_with_ints))
      return(fit_object)
    },
    .predict = function(task) {
      args <- self$params
      
      X_cut <- data.table(do.call(cbind, lapply(seq_along(breaks_list), function(i){
        x_name <- names(breaks_list)[i]
        x_breaks <- breaks_list[[i]]
        x <- task_pred$X[[x_name]] 
        if(min(x) < min(x_breaks)){
          x_breaks[1] <- min(x)
        }
        if(max(x) > max(x_breaks)){
          x_breaks[length(x_breaks)] <- max(x)
        }
        x_cut <- cut(x, breaks = x_breaks, labels = FALSE, include.lowest = TRUE)
        factor(x_cut, levels = levels_list[x_name])
      })))
      
      names(X_cut) <- names(breaks_list)
      
      training_names_X <- (fit_object$training_task$X)
      remaining_names <- training_names_X[!(names(training_names_X) %in% names(X_cut))]
      remaining_names <- remaining_names[remaining_names %in% names(task$X)]
      X_remaining <- data.table(do.call(cbind, lapply(remaining_names, function(i){
        x_levels <- levels_list[(remaining_names[i])]
        factor(task$X[[(remaining_names[i])]], levels = x_levels)
      })))
      
      X_pred <- cbind(X_cut, X_remaining)
      
      train_df_with_ints <- as.data.frame(
        model.matrix(
          data = task_pred$X, 
          object = formula))
      
      args <- list(
        object = private$.fit_object$fit_object, newx = as.matrix(df_with_ints), type = "response"
      )
      
      # set choice regularization penalty
      if (self$params$use_min) {
        args$s <- "lambda.min"
      } else {
        args$s <- "lambda.1se"
      }
      
      if (task$has_node("offset")) {
        if (private$.fit_object$fit_object$glmnet.fit$offset) {
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
