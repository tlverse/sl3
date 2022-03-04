#' The Super Learner Algorithm
#'
#' Learner that encapsulates the Super Learner algorithm. Fits metalearner on
#' cross-validated predictions from learners. Then forms a pipeline with the
#' learners.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom data.table setcolorder
#' @importFrom origami folds_vfold make_folds
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
#'   - \code{learners}: The "library" of user-specified algorithms for the
#'       super learner to consider as candidates.
#'   - \code{metalearner = NULL}: The metalearner to be fit on cross-validated
#'       predictions from the candidates. If \code{NULL}, the
#'       \code{\link{default_metalearner}} is used to construct a metalearner
#'       based on the \code{outcome_type} of the training \code{task}.
#'   - \code{cv_control = NULL}: Optional list of arguments that will be used to
#'       define a specific cross-validation fold structure for fitting the
#'       super learner. Intended for use in a nested cross-validation scheme,
#'       such as cross-validated super learner (\code{\link{CV_lrnr_sl}}) or
#'       when \code{Lrnr_sl} is considered in the list of candidate
#'       \code{learners} in another \code{Lrnr_sl}. Includes the arguments
#'       listed below, and any others to be passed to
#'       \code{\link[origami]{fold_funs}}:
#'       - \code{strata = NULL}: Optional column name to define stratified
#'           cross-validation folds. If \code{NULL} and if
#'           \code{task$outcome_type$type} is binary or categorical, then the
#'           default behavior is to consider stratified cross-validation, where
#'           the strata are defined with respect to the outcome. To override
#'           the default behavior, i.e., to not consider stratified
#'           cross-validation when \code{strata = NULL} and
#'           \code{task$outcome_type$type} is binary or categorical is not
#'           \code{NULL}, set \code{strata = "none"}.
#'       - \code{id}: Optional column name to define clustered cross-validation,
#'           so dependent units are placed together in the same training sets
#'           and in the same validation set. If \code{NULL} and
#'           \code{task$nodes$id} is not \code{NULL}, then the default behavior
#'           is to consider \code{task$nodes$id} for defining clustered
#'           cross-validation scheme. To override the default behavior, i.e.,
#'           to not consider clustered cross-validation when
#'           \code{task$nodes$id} is not \code{NULL}, set \code{id = "none"}.
#'       - \code{fold_fun}: A function indicating the \pkg{origami}
#'           cross-validation scheme to use, such as
#'           \code{\link[origami]{folds_vfold}} for V-fold cross-validation.
#'           See \code{\link[origami]{fold_funs}} for a list of possibilities.
#'           If \code{NULL} and if other \code{cv_control} arguments are
#'           specified, e.g., \code{V}, \code{strata} or \code{id}, then the
#'           default behavior is to set \code{fold_fun = folds_vfold}.
#'       - \code{...}: Other arguments to be passed to \code{fold_fun}, such as
#'           \code{V} for \code{fold_fun = folds_vfold}. See
#'           \code{\link[origami]{fold_funs}} for a list fold-function-specific
#'           possible arguments.
#'   - \code{keep_extra = TRUE}: Stores all sub-parts of the super learner
#'       computation. When \code{FALSE}, the resulting object has a memory
#'       footprint that is significantly reduced through the discarding of
#'       intermediary data structures.
#'   - \code{...}: Any additional parameters that can be considered by
#'       \code{\link{Lrnr_base}}.
#'
#' @examples
#' data(cpp_imputed)
#' covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
#' task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")
#' # this is just for illustrative purposes, not intended for real applications
#' # of the super learner!
#' glm_lrn <- Lrnr_glm$new()
#' ranger_lrn <- Lrnr_ranger$new()
#' lasso <- Lrnr_glmnet$new()
#' ensemble_sl <- Lrnr_sl$new(learners = list(glm_lrn, ranger_lrn, lasso))
#' ensemble_sl_fit <- ensemble_sl$train(task)
#' # example with cv_control, where Lrnr_sl included as a candidate
#' ensemble_sl2 <- Lrnr_sl$new(
#'   learners = list(glm_learner, ranger_learner, lasso),
#'   cv_control = list(fold_fun = folds_vfold, V = 5)
#' )
#' discrete_sl <- Lrnr_sl$new(
#'   learners = list(glm_lrn, ranger_lrn, lasso, ensemble_sl2),
#'   metalearner = Lrnr_cv_selector$new(loss_squared_error)
#' )
#' discrete_sl_fit <- discrete_sl$train(task)
#' # example with cv_control, where we use cross-validated super learner
#' cv_sl <- CV_lrnr_sl(
#'   lrnr_sl = ensemble_sl2, task = task, eval_fun = loss_squared_error
#' )
Lrnr_sl <- R6Class(
  classname = "Lrnr_sl", inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(learners, metalearner = "default", cv_control = NULL,
                          keep_extra = TRUE, ...) {

      # kludge to deal with stack as learners
      if (inherits(learners, "Stack")) {
        learners <- learners$params$learners
      }

      if (inherits(learners, "Lrnr_base")) {
        learners <- list(learners)
      }

      params <- list(
        learners = learners,
        metalearner = metalearner,
        cv_control = cv_control,
        keep_extra = keep_extra,
        ...
      )
      super$initialize(params = params, ...)
    },
    print = function() {
      if (!self$is_trained) {
        lrn_names <- lapply(self$params$learners, function(obj) obj$name)
        print("Super learner:")
        str(lrn_names)
      } else {
        fit_object <- private$.fit_object
        if (self$params$keep_extra) {

          # print cv_meta_fit only when coefficients are not shown in cv_risk
          if (is.null(names(self$coefficients))) {
            print(fit_object$cv_meta_fit)
          }

          # (cv risk estimates are stored to avoid unnecessary re-calculation)
          if (is.null(private$.cv_risk)) {
            if (is.null(private$.params$metalearner$params$eval_function)) {
              # try using eval function based on outcome type
              outcome_type <- self$training_outcome_type$type
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
            } else {
              eval_fun <- private$.params$metalearner$params$eval_function
            }

            private$.cv_risk <- self$cv_risk(eval_fun)
          }
          print("Cross-validated risk:")
          print(private$.cv_risk)
        } else {
          # just print the remaining full fit object when keep_extra = FALSE
          print(fit_object$full_fit$params$learners[[2]])
        }
      }
    },
    metalearner_fit = function() {
      self$assert_trained()
      return(private$.fit_object$cv_meta_fit$fit_object)
    },
    cv_risk = function(eval_fun, get_sl_revere_risk = FALSE) {
      # get risks for cv learners (nested cv)
      cv_stack_fit <- self$fit_object$cv_fit
      stack_risks <- cv_stack_fit$cv_risk(eval_fun)

      coefs <- self$coefficients
      if (!is.null(coefs)) {
        ordered_coefs <- coefs[match(stack_risks$learner, names(coefs))]
      } else {
        # metalearner did not provide coefficients.
        ordered_coefs <- rep(NA, length(stack_risks$learner))
      }
      set(stack_risks, , "coefficients", ordered_coefs)

      # make sure that coefficients is the second column, even if the
      # metalearner did not provide coefficients.
      data.table::setcolorder(
        stack_risks,
        c(names(stack_risks)[1], "coefficients")
      )

      if (get_sl_revere_risk) {
        # get risks for super learner ("revere" CV)
        sl_risk <- cv_risk(self, eval_fun)
        set(sl_risk, , "learner", "SuperLearner")

        # combine and return
        risks <- rbind(stack_risks, sl_risk)
        return(risks)
      } else {
        return(stack_risks)
      }
    },
    predict_fold = function(task, fold_number = "validation",
                            pred_unique_ts = FALSE) {
      fold_number <- interpret_fold_number(fold_number)
      revere_task <- task$revere_fold_task(fold_number)
      if (fold_number == "full") {
        preds <- self$predict(revere_task)
      } else {
        meta_task <- self$fit_object$cv_fit$chain_fold(
          revere_task,
          fold_number
        )
        preds <- self$fit_object$cv_meta_fit$predict(meta_task)

        if (pred_unique_ts) {
          ### Time-series addition:
          # Each time point gets an unique final prediction
          folds <- revere_task$folds
          index_val <- unlist(lapply(folds, function(fold) {
            fold$validation_set
          }))
          preds_unique <- unique(index_val)

          if (length(unique(index_val)) != length(index_val)) {
            # Average over the same predictions:
            preds <- data.table(index_val, preds)
            preds <- preds[, mean(preds), index_val]
            preds <- as.numeric(preds$V1)
          }
        }
      }

      return(preds)
    },
    update = function(task, drop_old = FALSE) {
      if (!self$is_trained) {
        return(self$train(task))
      }

      fit_object <- self$fit_object
      fit_object$cv_fit <- fit_object$cv_fit$update(task, drop_old = drop_old)

      # fit meta-learner
      fit_object$cv_meta_task <- fit_object$cv_fit$chain(task)
      fit_object$cv_meta_fit <-
        self$params$metalearner$train(fit_object$cv_meta_task)

      # construct full fit pipeline
      full_stack_fit <- fit_object$cv_fit$fit_object$full_fit
      full_stack_fit$custom_chain(drop_offsets_chain)

      full_fit <- make_learner(
        Pipeline, full_stack_fit,
        fit_object$cv_meta_fit
      )
      fit_object$full_fit <- full_fit

      new_object <- self$clone() # copy parameters, and whatever else
      new_object$set_train(fit_object, task)
      return(new_object)
    }
  ),
  active = list(
    name = function() {
      name <- paste("CV", self$params$learner$name, sep = "_")
    },
    coefficients = function() {
      self$assert_trained()
      return(coef(self$fit_object$cv_meta_fit))
    },
    learner_fits = function() {
      result <- self$fit_object$full_fit$learner_fits[[1]]$learner_fits
      return(result)
    }
  ),
  private = list(
    .properties = c("wrapper", "cv"),
    .cv_risk = NULL, # store risk estimates (avoid re-calculation on print)

    .train_sublearners = function(task) {
      # if we get a delayed task, evaluate it
      # TODO: this is a kludge:
      # ideally we'd have Lrnr_sl work on delayed tasks like other learners
      if (inherits(task, "Delayed")) {
        task <- task$compute()
      }

      cv_control <- self$params$cv_control
      if (is.null(cv_control) | is.null(unlist(cv_control))) {
        # TODO: this breaks if task is delayed
        folds <- task$folds
      } else {
        # initialize args for make_folds (cv_args) with cv_control ... args
        cv_args <- cv_control[!names(cv_control) %in% c("fold_fun", "strata", "id")]

        # set fold function
        if (is.null(cv_control$fold_fun)) {
          cv_args$fold_fun <- origami::folds_vfold
          sprintf(
            "Setting cv_control's fold_fun to folds_vfold. To override, ",
            "specify another cross-validation scheme from the origami package."
          )
        } else {
          cv_args$fold_fun <- cv_control$fold_fun
          if (class(cv_control$fold_fun) != "function") {
            stop(
              "The specified fold_fun is not a function. Make sure the fold ",
              "function is provided in the origami package. See the Lrnr_sl ",
              "example with cv_control specified, where it is shown how to ",
              "correctly specify fold_fun in cv_control."
            )
          }
        }

        # clustered cross-validation
        if (is.null(cv_control$id)) {
          if (task$has_node("id")) {
            sprintf(
              "Defining clustered cross-validation for Lrnr_sl according to ",
              "the id specified in the task, %s. To override this default ",
              "behavior, i.e., to not consider clustered cross-validation in ",
              "Lrnr_sl even though id is specified in the task, set ",
              "id = 'none' in the cv_control list.", task$nodes$id
            )
            cv_args$cluster_ids <- task$data[[task$nodes$id]]
          }
        } else {
          if (cv_control$id != "none") {
            if (cv_control$id %in% names(task$data)) {
              cv_args$cluster_ids <- task$data[[cv_control$id]]
            } else {
              warning(
                "The specified id in cv_control, ", paste0(cv_control$id),
                ", was not found as a column name in the task's data so ",
                "clustered cross-validation will not be considered."
              )
            }
          }
        }

        # stratified cross-validation
        if (!is.null(cv_control$strata)) {
          if (cv_control$strata %in% names(task$data)) {
            cv_args$strata_ids <- task$data[[cv_control$strata]]
          } else {
            warning(
              "The specified strata in cv_control, ", paste0(cv_control$strata),
              ", was not found as a column name in the task's data so ",
              "stratified cross-validation will not be considered."
            )
          }
        } else {
          if (cv_control$strata != "none" &
            task$outcome_type$type %in% c("binomial", "categorical")) {
            # stratified cross-validation folds for discrete outcomes
            cv_args$strata_ids <- task$Y
            sprintf(
              "Defining stratified cross-validation for Lrnr_sl according ",
              " to %s, as the outcome type is either binary or categorical. ",
              "To override this default behavior, i.e., to not consider ",
              "stratified cross-validation in Lrnr_sl even though the ",
              "outcome is discrete, set strata = 'none' in the cv_control ",
              "list.", task$nodes$outcome
            )
          }
        }


        # don't use stratified CV if clusters are not nested in strata
        if (!is.null(cv_args$cluster_ids) & !is.null(cv_args$strata_ids)) {
          is_nested <- all(rowSums(table(cv_args$cluster_ids, cv_args$strata_ids) > 0) == 1)
          if (!is_nested) {
            cv_args <- cv_args[!(names(cv_args) == "strata_ids")]
            warning(
              "Clusters, i.e., the ids, are not nested in the strata so ",
              "stratified cross-validation will not be considered."
            )
          }
          if (!is.null(cv_args$V)) {
            if (length(unique(cv_args$cluster_ids)) < cv_args$V) {
              cv_args <- cv_args[!(names(cv_args) == "cluster_ids")]
              warning(
                "There are less clusters, i.e., less unique ids, than V so ",
                "clustered cross-validation will not be considered."
              )
            }
            if (is_nested & length(unique(cv_args$strata_ids)) > cv_args$V) {
              cv_args <- cv_args[!(names(cv_args) == "strata_ids")]
              warning(
                "There are more strata than V so stratified cross-validation ",
                "will not be considered."
              )
            }
          }
        }
        folds <- do.call(origami::make_folds, cv_args)
      }

      # construct default metalearner if necessary
      metalearner <- self$params$metalearner
      if (is.character(metalearner) && (metalearner == "default")) {
        metalearner <- default_metalearner(task$outcome_type)
        private$.params$metalearner <- metalearner
      }

      # make stack and CV learner objects
      learners <- self$params$learners
      learner_stack <- do.call(Stack$new, list(learners))
      cv_stack <- Lrnr_cv$new(learner_stack, folds = folds, full_fit = TRUE)

      # TODO: read custom chain w/ better cv chain code
      # cv_stack$custom_chain(drop_offsets_chain)

      # fit stack on CV data
      cv_fit <- delayed_learner_train(cv_stack, task)

      # fit meta-learner
      cv_meta_task <- delayed_learner_fit_chain(cv_fit, task)
      cv_meta_fit <- delayed_learner_train(metalearner, cv_meta_task)

      # form full SL fit -- a pipeline with the stack fit to the full data,
      # and the metalearner fit to the CV predictions
      fit_object <- list(
        cv_fit = cv_fit, cv_meta_task = cv_meta_task,
        cv_meta_fit = cv_meta_fit
      )
      return(bundle_delayed(fit_object))
    },
    .train = function(task, trained_sublearners) {
      fit_object <- trained_sublearners

      # construct full fit pipeline
      full_stack_fit <- fit_object$cv_fit$fit_object$full_fit
      full_stack_fit$custom_chain(drop_offsets_chain)

      full_fit <- make_learner(
        Pipeline, full_stack_fit,
        fit_object$cv_meta_fit
      )
      fit_object$full_fit <- full_fit

      if (self$params$keep_extra) {
        keep <- names(fit_object)
      } else {
        keep <- c("full_fit")
        # TODO: replace learners with zero weight with smaller fit objects
        # coefs <- self$coefficients
        # nz_learners <- which(coefs > 0)
      }
      return(fit_object[keep])
    },
    .predict = function(task) {
      full_task <- task$revere_fold_task("full")
      predictions <- private$.fit_object$full_fit$base_predict(full_task)
      return(predictions)
    }
  )
)

#' Chain while dropping offsets
#'
#' Allows the dropping of offsets when calling the chain method. This is simply
#' a modified version of the chain method found in \code{Lrnr_base}. INTERNAL
#' USE ONLY.
#'
#' @param task An object of class \code{sl3_Task}.
#'
#' @keywords internal
#
drop_offsets_chain <- function(learner, task) {
  # pull out the validation task if we're in a revere context
  task <- task$revere_fold_task("validation")
  predictions <- learner$predict(task)
  predictions <- as.data.table(predictions)
  # Add predictions as new columns
  if (nrow(task$data) != nrow(predictions)) {
    # Gather validation indexes:
    val_index <- unlist(lapply(task$folds, function(fold) {
      fold$validation_set
    }))
    task <- task$subset_task(val_index)
    new_col_names <- task$add_columns(predictions, learner$fit_uuid)
  } else {
    new_col_names <- task$add_columns(predictions, learner$fit_uuid)
  }
  # new_covariates = union(names(predictions),task$nodes$covariates)
  return(task$next_in_chain(
    covariates = names(predictions),
    column_names = new_col_names,
    offset = NULL
  ))
}
