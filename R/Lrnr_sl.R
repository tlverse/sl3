#' @importFrom assertthat assert_that is.count is.flag
#' @importFrom origami make_folds cross_validate
#' @importFrom data.table data.table
#' @export
#' @rdname undocumented_learner
Lrnr_sl <- R6Class(classname = "Lrnr_sl", inherit = Lrnr_base, portable = TRUE,
                   class = TRUE,
  public = list(
    initialize = function(learners, metalearner, folds = NULL,
                          keep_extra = TRUE, ...) {
      # kludge to deal with stack as learners
      if (inherits(learners,"Stack")) {
        learners <- learners$params$learners
      }
      params = list(learners = learners, metalearner = metalearner,
                    folds = folds, keep_extra = keep_extra, ...)
      super$initialize(params = params, ...)
    },
    print = function() {
      lrn_names <- lapply(self$params$learners, function(obj) obj$name)
      print("SuperLearner:"); str(lrn_names)
      if (self$is_trained) {
        fit_object <- private$.fit_object
        print(fit_object$cv_meta_fit)
        print("Cross-validated risk (MSE, squared error loss):")
        print(self$cv_risk(loss_squared_error))
      }
    },
    metalearner_fit = function() {
      self$assert_trained()
      return(private$.fit_object$cv_meta_fit$fit_object)
    },
    cv_risk = function(loss_fun) {
      # warning("cv_risks are for demonstration purposes only. Don't trust these for now")
      cv_meta_task <- self$fit_object$cv_meta_task
      cv_meta_fit <- self$fit_object$cv_meta_fit
      losses <- cv_meta_task$X[, lapply(.SD, loss_fun, cv_meta_task$Y)]
      losses[, SuperLearner := loss_fun(cv_meta_fit$predict(), cv_meta_task$Y)]
      # multiply each loss (L(O_i)) by the weights (w_i):
      losses_by_id <- losses[, lapply(.SD, function(loss) cv_meta_task$weights *
                                      loss)]
      # for clustered data, this will first evaluate the mean weighted loss
      # within each cluster (subject) before evaluating SD
      losses_by_id <- losses_by_id[, lapply(.SD, function(loss) mean(loss,
                                                                     na.rm = TRUE)),
                                            by = cv_meta_task$id]
      losses_by_id[, "cv_meta_task" := NULL]
      # n_obs for clustered data (person-time observations), should be equal to
      # number of independent subjects
      n_obs <- nrow(losses_by_id)
      # evaluate risk SE for each learner incorporating: a) weights and b) using
      # the number of independent subjects
      se <- unlist((1 / sqrt(n_obs)) * losses_by_id[, lapply(.SD, sd,
                                                             na.rm = TRUE)])
      # get fold specific risks
      validation_means <- function(fold, losses, weight) {
        risks <- lapply(origami::validation(losses), weighted.mean,
                        origami::validation(weight))
        return(list(risks = as.data.frame(risks)))
      }
      # TODO: this ignores weights, square errors are also incorrect
      fold_risks <- origami::cross_validate(validation_means,
                                            cv_meta_task$folds, losses,
                                            cv_meta_task$weights)$risks
      fold_mean_risk <- apply(fold_risks, 2, mean)
      fold_min_risk <- apply(fold_risks, 2, min)
      fold_max_risk <- apply(fold_risks, 2, max)
      fold_SD <- apply(fold_risks, 2, sd)

      learner_names <- c(sapply(self$params$learners, "[[", "name"),
                         "SuperLearner")
      risk_dt <- data.table::data.table(learner = learner_names,
                                        mean_risk = fold_mean_risk,
                                        SE_risk = se,
                                        fold_SD = fold_SD,
                                        fold_min_risk = fold_min_risk,
                                        fold_max_risk = fold_max_risk)
      return(risk_dt)
    }
  ),
  active = list(
    name = function() {
      name = paste("CV", self$params$learner$name, sep = "_")
    }
  ),
  private = list(
    .properties = c("wrapper"),
    .pretrain = function(task) {
      # prefer folds from params, but default to folds from task
      folds <- self$params$folds
      if (is.null(folds)) {
        # TODO: this breaks if task is delayed
        folds <- task$folds
      }
      # make stack and CV learner objects
      learners <- self$params$learners
      learner_stack <- do.call(Stack$new, learners)
      cv_stack <- Lrnr_cv$new(learner_stack, folds = folds)

      # fit stack on CV data
      cv_fit <- delayed_learner_train(cv_stack, task)

      # fit meta-learner
      metalearner <- self$params$metalearner
      cv_meta_task <- delayed_learner_fit_chain(cv_fit, task)
      cv_meta_fit <- delayed_learner_train(metalearner, cv_meta_task)

      # refit stack on full data
      stack_fit <- delayed_learner_train(learner_stack, task)

      # form full SL fit -- a pipeline with the stack fit to the full data,
      # and the metalearner fit to the cv predictions
      full_fit <- delayed_make_learner(Pipeline, stack_fit, cv_meta_fit)
      fit_object <- list(cv_fit = cv_fit, cv_meta_task = cv_meta_task,
                         cv_meta_fit = cv_meta_fit, full_fit = full_fit)
      return(bundle_delayed(fit_object))
    },
    .train = function(task, pretrain) {
      # propagate stack errors from cross-validation to full refit
      fit_object <- pretrain
      cv_errors <- fit_object$cv_fit$fit_object$is_error
      fit_object$full_fit$fit_object$learner_fits[[1]]$update_errors(cv_errors)

      if (self$params$keep_extra) {
        keep = names(fit_object)
      } else {
        keep = c("full_fit")
      }
      return(fit_object[keep])
    },
    .predict = function(task) {
      predictions = private$.fit_object$full_fit$predict(task)
      return(predictions)
    }
  )
)

