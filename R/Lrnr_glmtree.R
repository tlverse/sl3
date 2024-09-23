#' Generalized Linear Model Trees
#'
#' This learner uses \code{\link[partykit]{glmtree}} from \pkg{partykit} to fit
#' recursive partitioning and regression trees in a generalized linear model.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stats predict
#'
#' @export
#'
#' @keywords data
#'
#' @return \code{\link[sl3]{Lrnr_base}} object with methods for training and
#'  prediction
#' @format \code{\link[R6]{R6Class}} object.
#'
#' @family Learners
#'
#' @section Parameters:
#'   - \code{formula}: An optional object of class \code{formula} (or one that
#'       can be coerced to that class), which a symbolic description of the
#'       generalized linear model to be fit. If not specified a main terms
#'       regression model will be supplied, with each covariate included as
#'       a term. Please consult \code{\link[partykit]{glmtree}} documentation
#'       for more information on its use of \code{formula}, and for a
#'       description on \code{formula} syntax consult the details of the
#'       \code{\link[stats]{glm}} documentation.
#'   - \code{...}: Other parameters passed to
#'       \code{\link[partykit]{mob_control}} or \code{\link[partykit]{glmtree}}
#'       that are not already specified in the \code{\link{sl3_Task}}. See its
#'       documentation for details.
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
#' glmtree_lrnr <- Lrnr_glmtree$new()
#' glmtree_fit <- glmtree_lrnr$train(cpp_task)
#' glmtree_preds <- glmtree_fit$predict()
Lrnr_glmtree <- R6Class(
  classname = "Lrnr_glmtree", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(formula = NULL,
                          ...) {
      params <- args_to_list()
      super$initialize(params = params)
    },
    # for learners that take formula as an argument, the function
    # process_formula that's defined in Lrnr_base needs to be redefined in
    # the learner like below
    process_formula = function(task) {
      return(task)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial", "weights", "offset"),
    .required_packages = c("partykit"),
    .train = function(task) {
      args <- self$params

      outcome_type <- self$get_outcome_type(task)
      args$family <- outcome_type$glm_family(return_object = TRUE)

      args$data <- task$data

      # check formula corresponds to task, if it's specified
      if (!is.null(args$formula)) {
        form <- args$formula
        if (!inherits(form, "formula")) form <- as.formula(form)

        if (task$has_node("offset") && is.null(attr(terms(form), "offset"))) {
          stop("Task has an offset; this needs to be specified as another term in the user-supplied formula")
        }

        # check response variable corresponds to outcome in task, if provided
        if (attr(terms(form), "response")) {
          if (!all.vars(form)[1] == task$nodes$outcome) {
            stop(paste0(
              "Outcome variable in formula ", all.vars(form)[1],
              " does not match the task's outcome ", task$nodes$outcome
            ))
          }
          formula_covars <- all.vars(form)[-1]
        } else {
          formula_covars <- all.vars(form)
        }
        # check that regressors in the formula are contained in the task
        if (!all(formula_covars %in% task$nodes$covariates)) {
          stop("Regressors in the formula are not covariates in task")
        }
      } else {
        if (task$has_node("offset")) {
          args$formula <- as.formula(paste(paste(task$nodes$outcome, paste("offset", "(", task$nodes$offset, ")", sep = ""), sep = "~"), paste(task$nodes$covariates, collapse = "+"), sep = "|"))
        } else {
          # create formula if it's not specified
          args$formula <- as.formula(paste(task$nodes$outcome, paste(task$nodes$covariates, collapse = "+"), sep = "~"))
        }
      }

      # only add weights and offset arguments if specified in task
      if (task$has_node("weights")) {
        args$weights <- task$weights
      }

      fit_object <- call_with_args(
        partykit::glmtree, args,
        other_valid = names(formals(partykit::mob_control))
      )
      return(fit_object)
    },
    .predict = function(task) {
      # get predictions
      predictions <- stats::predict(
        private$.fit_object,
        newdata = task$data
      )
      predictions <- as.numeric(predictions)
      return(predictions)
    }
  )
)
