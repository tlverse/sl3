utils::globalVariables(c("score"))

#' Variable Importance
#'
#' Function that takes a cross-validated fit (i.e., cross-validated learner
#' that has already been trained on a task), which could be a cross-validated
#' single learner or super learner, and generates a risk-based variable
#' importance score for either each covariate or each group of covariates in
#' the task.  This function outputs a \code{data.table}, where each row
#' corresponds to the risk difference or the risk ratio between the following
#' two risks: the risk when a covariate (or group of covariates) is permuted or
#' removed, and the original risk (i.e., when all covariates are included as
#' they were in the observed data). A higher risk ratio/difference corresponds
#' to a more important covariate/group. A plot can be generated from the
#' returned \code{data.table} by calling companion function
#' \code{\link{importance_plot}}.
#'
#' @export
#'
#' @name importance
#' @rdname importance
#' @keywords variable importance
#'
#' @importFrom data.table data.table setorder setnames
#'
#' @return A \code{data.table} of variable importance for each covariate.
#'
#' @param fit A trained cross-validated (CV) learner (such as a CV stack or
#'  super learner), from which cross-validated predictions can be generated.
#' @param eval_fun The evaluation function (risk or loss function) for
#'  evaluating the risk. Defaults vary based on the outcome type, matching
#'  defaults in \code{\link{default_metalearner}}. See
#'  \code{\link{loss_functions}} and \code{\link{risk_functions}} for options.
#'  Default is \code{NULL}.
#' @param fold_number The fold number to use for obtaining the predictions from
#'  the fit. Either a positive integer for obtaining predictions from a
#'  specific fold's fit; \code{"full"} for obtaining predictions from a fit on
#'  all of the data, or \code{"validation"} (default) for obtaining
#'  cross-validated predictions, where the data used for training and
#'  prediction never overlaps across the folds. Note that if a positive integer
#'  or \code{"full"} is supplied here then there will be overlap between the
#'  data used for training and validation, so \code{fold_number ="validation"}
#'  is recommended.
#' @param type Which method should be used to obscure the relationship between
#'  each covariate / covariate group and the outcome? When \code{type} is
#'  \code{"remove"} (default), each covariate / covariate group is removed one
#'  at a time from the task; the cross-validated learner is refit to this
#'  modified task; and finally, predictions are obtained from this refit. When
#'  \code{type} is \code{"permute"}, each covariate / covariate group is
#'  permuted (sampled without replacement) one at a time, and then predictions
#'  are obtained from this modified data.
#' @param importance_metric Either \code{"ratio"} or \code{"difference"}
#'  (default). For each covariate / covariate group, \code{"ratio"} returns the
#'  risk of the permuted/removed covariate / covariate group divided by
#'  observed/original risk (i.e., the risk with all covariates as they existed
#'  in the sample) and \code{"difference"} returns the difference between the
#'  risk with the permuted/removed covariate / covariate group and the observed
#'  risk.
#' @param covariate_groups Optional named list covariate groups which will
#'  invoke variable importance evaluation at the group-level, by
#'  removing/permuting all covariates in the same group together. If covariates
#'  in the task are not specified in the list of groups, then those covariates
#'  will be added as additional single-covariate groups.
#'
#' @examples
#' # define ML task
#' data(cpp_imputed)
#' covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
#' task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")
#'
#' # build relatively fast learner library (not recommended for real analysis)
#' lasso_lrnr <- Lrnr_glmnet$new()
#' glm_lrnr <- Lrnr_glm$new()
#' ranger_lrnr <- Lrnr_ranger$new()
#' lrnrs <- c(lasso_lrnr, glm_lrnr, ranger_lrnr)
#' names(lrnrs) <- c("lasso", "glm", "ranger")
#' lrnr_stack <- make_learner(Stack, lrnrs)
#'
#' # instantiate SL with default metalearner
#' sl <- Lrnr_sl$new(lrnr_stack)
#' sl_fit <- sl$train(task)
#'
#' importance_result <- importance(sl_fit)
#' importance_result
#'
#' # importance with groups of covariates
#' groups <- list(
#'   scores = c("apgar1", "apgar5"),
#'   maternal = c("parity", "mage", "meducyrs")
#' )
#' importance_result_groups <- importance(sl_fit, covariate_groups = groups)
#' importance_result_groups
importance <- function(fit, eval_fun = NULL,
                       fold_number = "validation",
                       type = c("remove", "permute"),
                       importance_metric = c("difference", "ratio"),
                       covariate_groups = NULL) {

  # check fit is trained
  if (!fit$is_trained) {
    stop("Fit is not trained.")
  }

  ################################ set defaults ################################
  importance_metric <- match.arg(importance_metric)
  type <- match.arg(type)

  if (is.null(eval_fun)) {
    outcome_type <- fit$training_task$outcome_type$type
    if (outcome_type %in% c("constant", "binomial")) {
      eval_fun <- loss_squared_error
    } else if (outcome_type == "categorical") {
      eval_fun <- loss_loglik_multinomial
    } else if (outcome_type == "continuous") {
      eval_fun <- loss_squared_error
    } else if (outcome_type == "multivariate") {
      eval_fun <- loss_squared_error_multivariate
    } else {
      stop(paste0(
        "No default eval_fun for outcome type ", outcome_type,
        ". Please specify your own."
      ))
    }
  }

  ########################### extract nodes and data ###########################
  task <- fit$training_task
  d <- task$data
  X <- task$nodes$covariates
  Y <- task$Y
  weights <- task$weights

  ############################ check covariate groups ##########################
  if (!is.null(covariate_groups)) {
    if (!is.list(covariate_groups)) {
      stop("Covariate groups must be a list.")
    }

    # check that all covariates in the groups are also in the task's covariates
    if (!all(unlist(covariate_groups) %in% X)) {
      stop("Groups contain covariates that are not in the task's covariates.")
    }

    # check if all covariates in task are in groups, and if not add them
    if (!all(X %in% unlist(covariate_groups))) {
      missingX <- as.list(X[which(!X %in% unlist(covariate_groups))])
      names(missingX) <- X[which(!X %in% unlist(covariate_groups))]
      covariate_groups <- c(covariate_groups, missingX)
    }
    # check that groups with more than one covariate are named, and add name to
    # unnamed groups but only if unnamed group is a single covariate
    if (any(is.null(names(covariate_groups))) |
      any(names(covariate_groups) == "") |
      any(is.na(names(covariate_groups)))) {
      no_name <- unique(which(
        is.null(names(covariate_groups)) | names(covariate_groups) == "" |
          is.na(names(covariate_groups))
      ))
      if (any(sapply(covariate_groups[unique(no_name)], length)) != 1) {
        stop("Covariate groups with more than one covariate must be named.")
      } else if (all(sapply(covariate_groups[unique(no_name)], length)) == 1) {
        names(covariate_groups[no_name]) <- unlist(covariate_groups[no_name])
      }
    }
    X <- covariate_groups # for convenience in remaining parts of function
  } else {
    names(X) <- X
  }

  ########################## risk under observed data ##########################
  original_pred <- fit$predict_fold(task, fold_number = fold_number)
  original_eval <- eval_fun(original_pred, Y)
  if (!is.null(attr(original_eval, "loss")) && !attr(original_eval, "loss")) {
    original_risk <- original_eval
  } else {
    original_losses <- original_eval
    original_risk <- weighted.mean(original_losses, weights)
  }

  ######################## list of importance results ##########################
  res_list <- lapply(X, function(x) {
    # get the risk when the covariate/group is permuted/removed
    if (type == "permute") {
      # get the permutation rows
      perm <- sample(1:nrow(d), nrow(d))
      # permute x (the covariate/group), and name it as the original x
      x_perm <- d[perm, x, with = FALSE]
      data.table::setnames(x_perm, x)
      # replace original x with permuted x, and update task with permuted x
      x_perm_name <- task$add_columns(x_perm)
      task_x_perm <- task$next_in_chain(column_names = x_perm_name)
      # obtain predictions & risk on the new task with permuted x
      x_perm_pred <- fit$predict_fold(task_x_perm, fold_number = fold_number)
      x_perm_eval <- eval_fun(x_perm_pred, Y)
      if (!is.null(attr(original_eval, "loss")) && !attr(original_eval, "loss")) {
        no_x_risk <- x_perm_eval
      } else {
        no_x_losses <- x_perm_eval
        no_x_risk <- weighted.mean(no_x_losses, weights)
      }
    } else if (type == "remove") {
      # modify learner to not include x (covariate/group)
      if (!is.null(covariate_groups)) {
        x_rm_covars <- setdiff(unlist(X), x)
      } else {
        x_rm_covars <- setdiff(X, x)
      }
      x_rm_lrnr <- fit$reparameterize(list(covariates = x_rm_covars))
      x_rm_fit <- x_rm_lrnr$train(task)
      x_rm_pred <- x_rm_fit$predict_fold(task, fold_number = fold_number)
      x_rm_eval <- eval_fun(x_rm_pred, Y)
      if (!is.null(attr(original_eval, "loss")) && !attr(original_eval, "loss")) {
        no_x_risk <- x_rm_eval
      } else {
        no_x_losses <- x_rm_eval
        no_x_risk <- weighted.mean(no_x_losses, weights)
      }
    }

    # evaluate importance
    if (importance_metric == "ratio") {
      result <- no_x_risk / original_risk
    } else if (importance_metric == "difference") {
      result <- no_x_risk - original_risk
    }
    return(result)
  })

  ############################## prep output ###################################
  # importance results ordered by decreasing importance
  result <- data.table::data.table(covariate = names(X), metric = unlist(res_list))
  if (!is.null(covariate_groups)) {
    colnames(result)[1] <- "covariate_group"
  }
  data.table::setorder(result, -metric)

  # name the importance metric appropriately
  metric_name <- paste0("risk_", importance_metric)
  if (!is.null(attr(original_eval, "name"))) {
    metric_name <- gsub("risk", attr(original_eval, "name"), metric_name)
  }
  colnames(result)[2] <- metric_name
  return(result)
}

#' Variable Importance Plot
#'
#' @param x The two-column \code{data.table} returned by
#'  \code{\link{importance}}, where the first column is the covariate/groups
#'  and the second column is the importance score.
#' @param nvar The maximum number of predictors to be plotted. Defaults to the
#'  minimum between 30 and the number of rows in \code{x}.
#'
#' @return A \code{\link[ggplot2]{ggplot}} of variable importance.
#'
#' @importFrom ggplot2 ggplot geom_point coord_flip scale_x_discrete labs
#' @importFrom data.table data.table
#'
#' @rdname importance_plot
#' @name importance_plot
#' @keywords variable importance
#'
#' @export
#'
#' @examples
#' # define ML task
#' data(cpp_imputed)
#' covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
#' task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")
#'
#' # build relatively fast learner library (not recommended for real analysis)
#' lasso_lrnr <- Lrnr_glmnet$new()
#' glm_lrnr <- Lrnr_glm$new()
#' ranger_lrnr <- Lrnr_ranger$new()
#' lrnrs <- c(lasso_lrnr, glm_lrnr, ranger_lrnr)
#' names(lrnrs) <- c("lasso", "glm", "ranger")
#' lrnr_stack <- make_learner(Stack, lrnrs)
#'
#' # instantiate SL with default metalearner
#' sl <- Lrnr_sl$new(lrnr_stack)
#' sl_fit <- sl$train(task)
#' importance_result <- importance(sl_fit)
#' importance_plot(importance_result)
importance_plot <- function(x, nvar = min(30, nrow(x))) {
  # get the importance metric
  xlab <- colnames(x)[2]

  # sort by increasing importance
  x <- x[order(-x[, 2]), ]
  # subset to include most at most nvar
  x <- x[1:(min(nvar, nrow(x))), ]

  # format for ggplot
  d <- data.table::data.table(
    vars = factor(x[[1]], levels = x[[1]]), score = x[[2]]
  )

  ggplot2::ggplot(d, aes(x = vars, y = score)) +
    ggplot2::geom_point() +
    ggplot2::ylim(c(min(d$score), max(d$score))) +
    ggplot2::labs(x = "", y = xlab, title = "sl3 Variable Importance Plot") +
    ggplot2::scale_x_discrete(limits = rev(levels(d$covs))) +
    ggplot2::coord_flip()
}
