#' Variable Importance
#'
#' Function that takes a cross-validated fit (i.e., cross-validated learner that
#' has already been trained on a task), which could be a cross-validated single
#' learner or super learner, to generate a risk-based variable importance
#' measure for each covariate in the trained task, or for each group of
#' covariates. This function outputs a \code{data.table}, where each row
#' corresponds to the risk difference or the risk ratio between the following
#' two risks: the risk when a covariate (or group of covariates) is permuted or
#' removed, and the original risk (i.e., when all covariates are included as
#' they were in the observed data). A higher risk ratio/difference corresponds
#' to a more important covariate/group. A plot can be generated from the
#' returned \code{data.table} by calling companion function
#' \code{plot_importance}.
#'
#' @param fit A trained cross-validated learner (e.g., cv stack, super learner),
#'  from which cross-validated predictions can be generated.
#' @param eval_fun The evaluation function (risk or loss function) for
#'  evaluating the risk. Defaults vary based on the outcome type: squared error
#'  loss for continuous outcomes, and negative log-likelihood loss for discrete
#'  outcomes. See \code{\link{loss_functions}} and \code{\link{risk_functions}}
#'  for options.
#' @param fold_number The fold number to use for obtaining the predictions
#'  from the fit. Either a positive integer for obtaining predictions from a
#'  specific fold's fit; \code{"full"} for obtaining predictions from a fit on
#'  all of the data, or \code{"validation"} (default) for obtaining
#'  cross-validated predictions, where the data used for training and prediction
#'  never overlaps across the folds. Note that if a positive integer or
#'  \code{"full"} is supplied, then there will be overlap between the  data
#'  used for training and prediction.
#' @param type Which method should be used to obscure the relationship between
#'  the covariate and the outcome. When type is \code{"remove"} (default), each
#'  covariate is removed from the task and the cross-validated learner is refit
#'  to this modified task and then predictions are obtained from this refit.
#'  When type is \code{"permute"}, each covariate is permuted (sampled without
#'  replacement), and then predictions are obtained from this modified data with
#'  one permuted covariate.
#' @param importance_metric Either \code{"ratio"} (default), which returns
#'  the risk with the permuted/removed X divided by observed risk with all X; or
#'  \code{"difference"}, which returns the difference between the risk with the
#'  permuted/removed X and the observed risk.
#' @param covariate_groups Optional named list covariate groups which will
#'  invoke variable importance evaluation at the group-level, by
#'  removing/permuting all covariates in the same group together. If covariates
#'  in the task are not specified in the list of groups, then those covariates
#'  will be added as additional single-covariate groups.
#' @param B Optional integer specifying the number of permutations to average
#'  the permuted risk over when \code{type = "permute"}.
#' @param cores Number of cores for parallelizing the \code{B} permutations.
#'
#' @importFrom data.table data.table
#' @importFrom parallel mclapply detectCores
#'
#' @return A \code{data.table} of variable importance for each covariate.
#'
#' @name importance
#' @keywords variable importance
#'
#' @export
importance <- function(fit, eval_fun = NULL, fold_number = "validation",
                       type = c("remove", "permute"),
                       importance_metric = c("ratio", "difference"),
                       covariate_groups = NULL, B = 100L,
                       cores = parallel::detectCores() - 1) {

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
      eval_fun <- loss_loglik_binomial
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
  if (!is.null(attr(original_eval, "risk"))) {
    if (!is.null(attr(original_eval, "transform"))) {
      transform_type <- attr(original_eval, "transform")
      original_eval <- transform_risk(original_eval, transform_type)
      # note that eval now is not a risk, since it's been transformed,
      # but we keep the name as is to streamline the functionality
    }
    original_risk <- original_eval
  } else {
    original_losses <- original_eval
    original_risk <- mean(original_losses)
  }

  ######################## list of importance results ##########################
  res_list <- lapply(seq_along(X), function(i) {
    # get the relevant group/covariate
    if (!is.null(covariate_groups)) {
      x <- X[[i]]
    } else {
      x <- X[i]
    }
    # get the risk when the covariate/group is permuted/removed
    if (type == "permute") {
      seeds <- sample(11:2^15, B)
      no_x_risk <- mean(unlist(parallel::mclapply(seeds, function(b) {
        set.seed(b)
        # get the permutation rows
        perm <- sample(1:nrow(d), nrow(d))
        # permute x (the covariate/group), and name it as the original x
        x_perm <- data.table(d[perm, x, with = FALSE])
        names(x_perm) <- x
        # replace original x with permuted x, and update task with permuted x
        x_perm_name <- task$add_columns(x_perm)
        task_x_perm <- task$next_in_chain(column_names = x_perm_name)
        # obtain predictions & risk on the new task with permuted x
        x_perm_pred <- fit$predict_fold(task_x_perm, fold_number)
        x_perm_eval <- eval_fun(x_perm_pred, Y)
        if (!is.null(attr(x_perm_eval, "risk"))) {
          if (!is.null(attr(x_perm_eval, "transform"))) {
            x_perm_eval <- transform_risk(x_perm_eval, transform_type)
          }
          no_x_risk <- x_perm_eval
        } else {
          no_x_losses <- x_perm_eval
          no_x_risk <- mean(no_x_losses)
        }
        return(no_x_risk)
      }, mc.cores = cores)))
    } else if (type == "remove") {
      # modify learner to not include x (covariate/group)
      if (!is.null(covariate_groups)) {
        x_rm_covars <- setdiff(unlist(X), x)
      } else {
        x_rm_covars <- setdiff(X, x)
      }
      x_rm_lrnr <- fit$reparameterize(list(covariates = x_rm_covars))
      x_rm_fit <- x_rm_lrnr$train(task)
      x_rm_pred <- x_rm_fit$predict_fold(task, fold_number)
      x_rm_eval <- eval_fun(x_rm_pred, Y)
      if (!is.null(attr(x_rm_eval, "risk"))) {
        if (!is.null(attr(x_rm_eval, "transform"))) {
          x_rm_eval <- transform_risk(x_rm_eval, transform_type)
        }
        no_x_risk <- x_rm_eval
      } else {
        no_x_losses <- x_rm_eval
        no_x_risk <- mean(no_x_losses)
      }
    }

    # evaluate importance
    if (importance_metric == "ratio") {
      result <- no_x_risk / original_risk
    } else if (importance_metric == "difference") {
      result <- no_x_risk - original_risk
    }

    if (type == "permute") {
      result <- list("result" = result, "RNGseeds" = seeds) # reproducibility
    } else if (type == "remove") {
      result <- list("result" = result)
    }

    return(result)
  })

  if (type == "permute") {
    RNGseeds <- data.table(do.call(cbind, lapply(res_list, "[[", "RNGseeds")))
    colnames(RNGseeds) <- names(X)
  }
  res_list <- lapply(res_list, "[[", "result")

  ############################## prep output ###################################
  # importance results ordered by decreasing importance
  result <- data.table(covariate = names(X), metric = unlist(res_list))
  if (!is.null(covariate_groups)) {
    colnames(result)[1] <- "covariate_group"
  }
  result <- result[order(-result$metric), ]

  # name the importance metric appropriately
  metric_name <- paste0("risk_", importance_metric)
  if (!is.null(attr(original_eval, "name"))) {
    metric_name <- gsub("risk", attr(original_eval, "name"), metric_name)
  }
  colnames(result)[2] <- metric_name

  if (type == "permute") {
    result <- list("result" = result, "RNGseeds" = RNGseeds)
  }

  return(result)
}

#' Variable Importance Plot
#'
#' @param x The 2-column \code{data.table} returned by \code{importance}, where
#'  the first column is the coviariate/groups and the second column is the
#'  importance metric.
#' @param nvar The maximum number of predictors to be plotted. Defaults to 30.
#' @param ... Other parameters passed to \code{\link[graphics]{dotchart}}.
#'
#' @return A \code{\link[graphics]{dotchart}} of variable importance.
#'
#' @importFrom graphics dotchart
#'
#' @name importance_plot
#' @keywords variable importance
#'
#' @export
importance_plot <- function(x, nvar = min(30, nrow(x)), ...) {
  if (is.list(x)) {
    x <- x$result
  }

  # get the importance metric
  xlab <- colnames(x)[2]

  # sort by decreasing importance
  x_sorted <- x[order(-x[, 2])]
  # subset to include most at most nvar
  x_sorted <- x_sorted[1:(min(nvar, nrow(x_sorted))), ]
  # sort by increasing importance
  x_sorted <- x_sorted[order(x_sorted[, 2])]

  # make dotchart with most important variables on top
  # x_sorted[[2]] is importance scores & x_sorted[[1]] is covariate names
  dotchart(
    x = x_sorted[[2]], labels = x_sorted[[1]],
    xlab = xlab, ylab = "",
    xlim = c(min(x_sorted[[2]]), max(x_sorted[[2]])), ...
  )
}
