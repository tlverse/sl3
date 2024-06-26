#' Semiparametric Generalized Linear Models
#'
#' This learner provides fitting procedures for semiparametric generalized
#' linear models using a specified baseline learner and
#' \code{\link[stats]{glm.fit}}. Models of the form
#' \code{linkfun(E[Y|A,W]) = linkfun(E[Y|A=0,W]) + A * f(W)} are supported,
#' where \code{A} is a binary or continuous interaction variable, \code{W} are
#' all of the covariates in the task excluding the interaction variable, and
#' \code{f(W)} is a user-specified parametric function of the
#' non-interaction-variable covariates (e.g.,
#' \code{f(W) = model.matrix(formula_sp, W)}). The baseline function
#' \code{E[Y|A=0,W]} is fit using a user-specified learner, possibly pooled
#' over values of interaction variable \code{A}, and then projected onto the
#' semiparametric model.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
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
#'   - \code{formula_parametric = NULL}: A \code{\link[stats]{formula}} object
#'       specifying the parametric function of the non-interaction-variable
#'       covariates.
#'   - \code{lrnr_baseline}: A baseline learner for
#'       estimation of the nonparametric component. This can be pooled or
#'       unpooled by specifying \code{return_matrix_predictions}.
#'   - \code{interaction_variable = NULL}: An interaction variable name
#'       present in the task's data that will be used to multiply by the
#'       design matrix generated by \code{formula_sp}. If \code{NULL} (default)
#'       then the interaction variable is treated identically \code{1}. When
#'       this learner is used for estimation of the outcome regression in an
#'       effect estimation procedure (e.g., when using \code{sl3} within
#'       package \code{tmle3}), it is recommended that
#'       \code{interaction_variable} be set as the name of the treatment
#'       variable.
#'   - \code{family = NULL}: A family object whose link function specifies the
#'       type of semiparametric model. For
#'       partially-linear least-squares regression,
#'       partially-linear logistic regression, and
#'       partially-linear log-linear regression \code{family} should be set to
#'       \code{guassian()}, \code{binomial()}, and \code{poisson()},
#'       respectively.
#'   - \code{append_interaction_matrix = TRUE}: Whether \code{lrnr_baseline}
#'       should be fit on \code{cbind(task$X,A*V)}, where \code{A} is the
#'       \code{interaction_variable} and \code{V} is the design matrix obtained
#'       from \code{formula_sp}. Note that if  \code{TRUE} (default) the
#'       resulting estimator will be projected onto the semiparametric model
#'       using \code{\link[stats]{glm.fit}}. If \code{FALSE} and
#'       \code{interaction_variable} is binary, the semiparametric model is
#'       learned by stratifying on \code{interaction_variable}; Specifically,
#'       \code{lrnr_baseline} is used to estimate \code{E[Y|A=0,W]} by
#'       subsetting to only observations with \code{A = 0}, i.e., subsetting to
#'       only observations with \code{interaction_variable = 0}, and where
#'       \code{W} are the other covariates in the task that are not the
#'       \code{interaction_variable}. In the binary \code{interaction_variable}
#'       case, setting \code{append_interaction_matrix = TRUE} allows one to
#'       pool the learning across treatment arms and can enhance performance of
#'       additive models.
#'   - \code{return_matrix_predictions = FALSE}: Whether to return a matrix
#'       output with three columns being \code{E[Y|A=0,W]}, \code{E[Y|A=1,W]},
#'       \code{E[Y|A,W]} in the learner's \code{fit_object}, where \code{A} is
#'       the \code{interaction_variable} and \code{W} are the other covariates
#'       in the task that are not the \code{interaction_variable}. Only used
#'       if the \code{interaction_variable} is binary.
#'   - \code{...}: Any additional parameters that can be considered by
#'       \code{\link{Lrnr_base}}.
#'
#' @examples
#' \dontrun{
#' # simulate some data
#' set.seed(459)
#' n <- 200
#' W <- runif(n, -1, 1)
#' A <- rbinom(n, 1, plogis(W))
#' Y_continuous <- rnorm(n, mean = A + W, sd = 0.3)
#' Y_binary <- rbinom(n, 1, plogis(A + W))
#' Y_count <- rpois(n, exp(A + W))
#' data <- data.table::data.table(W, A, Y_continuous, Y_binary, Y_count)
#'
#' # Make tasks
#' task_continuous <- sl3_Task$new(
#'   data,
#'   covariates = c("A", "W"), outcome = "Y_continuous"
#' )
#' task_binary <- sl3_Task$new(
#'   data,
#'   covariates = c("A", "W"), outcome = "Y_binary"
#' )
#' task_count <- sl3_Task$new(
#'   data,
#'   covariates = c("A", "W"), outcome = "Y_count",
#'   outcome_type = "continuous"
#' )
#'
#' formula_sp <- ~ 1 + W
#'
#' # fit partially-linear regression with append_interaction_matrix = TRUE
#' set.seed(100)
#' lrnr_glm_sp_gaussian <- Lrnr_glm_semiparametric$new(
#'   formula_sp = formula_sp, family = gaussian(),
#'   lrnr_baseline = Lrnr_glm$new(),
#'   interaction_variable = "A", append_interaction_matrix = TRUE
#' )
#' lrnr_glm_sp_gaussian <- lrnr_glm_sp_gaussian$train(task_continuous)
#' preds <- lrnr_glm_sp_gaussian$predict(task_continuous)
#' beta <- lrnr_glm_sp_gaussian$fit_object$coefficients
#' # in this case, since append_interaction_matrix = TRUE, it is equivalent to:
#' V <- model.matrix(formula_sp, task_continuous$data)
#' X <- cbind(task_continuous$data[["W"]], task_continuous$data[["A"]] * V)
#' X0 <- cbind(task_continuous$data[["W"]], 0 * V)
#' colnames(X) <- c("W", "A", "A*W")
#' Y <- task_continuous$Y
#' set.seed(100)
#' beta_equiv <- coef(glm(X, Y, family = "gaussian"))[c(3, 4)]
#' # actually, the glm fit is projected onto the semiparametric model
#' # with glm.fit, no effect in this case
#' print(beta - beta_equiv)
#' # fit partially-linear regression w append_interaction_matrix = FALSE`
#' set.seed(100)
#' lrnr_glm_sp_gaussian <- Lrnr_glm_semiparametric$new(
#'   formula_sp = formula_sp, family = gaussian(),
#'   lrnr_baseline = Lrnr_glm$new(family = gaussian()),
#'   interaction_variable = "A",
#'   append_interaction_matrix = FALSE
#' )
#' lrnr_glm_sp_gaussian <- lrnr_glm_sp_gaussian$train(task_continuous)
#' preds <- lrnr_glm_sp_gaussian$predict(task_continuous)
#' beta <- lrnr_glm_sp_gaussian$fit_object$coefficients
#' # in this case, since append_interaction_matrix = FALSE, it is equivalent to
#' # the following
#' cntrls <- task_continuous$data[["A"]] == 0 # subset to control arm
#' V <- model.matrix(formula_sp, task_continuous$data)
#' X <- cbind(rep(1, n), task_continuous$data[["W"]])
#' Y <- task_continuous$Y
#' set.seed(100)
#' beta_Y0W <- lrnr_glm_sp_gaussian$fit_object$lrnr_baseline$fit_object$coefficients
#' # subset to control arm
#' beta_Y0W_equiv <- coef(
#'   glm.fit(X[cntrls, , drop = F], Y[cntrls], family = gaussian())
#' )
#' EY0 <- X %*% beta_Y0W
#' beta_equiv <- coef(glm.fit(A * V, Y, offset = EY0, family = gaussian()))
#' print(beta_Y0W - beta_Y0W_equiv)
#' print(beta - beta_equiv)
#'
#' # fit partially-linear logistic regression
#' lrnr_glm_sp_binomial <- Lrnr_glm_semiparametric$new(
#'   formula_sp = formula_sp, family = binomial(),
#'   lrnr_baseline = Lrnr_glm$new(), interaction_variable = "A",
#'   append_interaction_matrix = TRUE
#' )
#' lrnr_glm_sp_binomial <- lrnr_glm_sp_binomial$train(task_binary)
#' preds <- lrnr_glm_sp_binomial$predict(task_binary)
#' beta <- lrnr_glm_sp_binomial$fit_object$coefficients
#'
#' # fit partially-linear log-link (relative-risk) regression
#' # Lrnr_glm$new(family = "poisson") setting requires that lrnr_baseline
#' # predicts nonnegative values. It is recommended to use poisson
#' # regression-based learners.
#' lrnr_glm_sp_poisson <- Lrnr_glm_semiparametric$new(
#'   formula_sp = formula_sp, family = poisson(),
#'   lrnr_baseline = Lrnr_glm$new(family = "poisson"),
#'   interaction_variable = "A",
#'   append_interaction_matrix = TRUE
#' )
#' lrnr_glm_sp_poisson <- lrnr_glm_sp_poisson$train(task_count)
#' preds <- lrnr_glm_sp_poisson$predict(task_count)
#' beta <- lrnr_glm_sp_poisson$fit_object$coefficients
#' }
Lrnr_glm_semiparametric <- R6Class(
  classname = "Lrnr_glm_semiparametric", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(formula_sp,
                          lrnr_baseline,
                          interaction_variable = NULL,
                          family = NULL,
                          append_interaction_matrix = TRUE,
                          return_matrix_predictions = FALSE,
                          ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    },
    process_formula = function(task) {
      return(task)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial", "weights"),
    .train = function(task) {
      args <- self$params
      append_interaction_matrix <- args$append_interaction_matrix

      interaction_variable <- args$interaction_variable
      if (is.null(interaction_variable)) {
        A <- rep(1, task$nrow)
      } else {
        A <- unlist(task$get_data(, interaction_variable))
      }
      if (!all(A %in% c(0, 1)) && !is.null(interaction_variable)) {
        binary_interaction_variable <- FALSE
      } else {
        binary_interaction_variable <- TRUE
      }

      lrnr_baseline <- args$lrnr_baseline
      formula <- args$formula_sp

      outcome_type <- self$get_outcome_type(task)
      family <- args$family
      if (is.null(family)) {
        family <- outcome_type$glm_family(return_object = TRUE)
      }

      # interaction design matrix
      Y <- task$Y
      V <- model.matrix(formula, task$data)
      colnames(V) <- paste0("V", 1:ncol(V))

      covariates <- setdiff(task$nodes$covariates, interaction_variable)

      if (!append_interaction_matrix && binary_interaction_variable) {
        task_baseline <- task$next_in_chain(covariates = covariates)
        lrnr_baseline <- lrnr_baseline$train(task_baseline[A == 0])
        Q0 <- lrnr_baseline$predict(task_baseline)
        beta <- suppressWarnings(coef(glm.fit(
          A * V, Y,
          offset = family$linkfun(Q0), intercept = FALSE,
          weights = task$weights, family = family
        )))
        Q1 <- family$linkinv(family$linkfun(Q0) + V %*% beta)
        Q <- ifelse(A == 1, Q1, Q0)
      } else {
        if (append_interaction_matrix) {
          AV <- as.data.table(A * V)
          X <- cbind(task$X[, covariates, with = F], AV)
          X0 <- cbind(task$X[, covariates, with = F], 0 * V)
        } else {
          X <- cbind(task$X[, covariates, with = F], A)
          X0 <- cbind(task$X[, covariates, with = F], A * 0)
        }
        column_names <- task$add_columns(X)
        task_baseline <- task$next_in_chain(
          covariates = colnames(X), column_names = column_names
        )
        column_names <- task$add_columns(X0)
        task_baseline0 <- task$next_in_chain(
          covariates = colnames(X0), column_names = column_names
        )
        lrnr_baseline <- lrnr_baseline$train(task_baseline)
        Q <- lrnr_baseline$predict(task_baseline)
        Q0 <- lrnr_baseline$predict(task_baseline0)

        # project onto model
        beta <- suppressWarnings(coef(glm.fit(
          A * V, Q,
          offset = family$linkfun(Q0), intercept = FALSE,
          weights = task$weights, family = family
        )))
      }

      fit_object <- list(
        coefficients = beta, lrnr_baseline = lrnr_baseline,
        covariates = covariates, family = family, formula = formula,
        append_interaction_matrix = append_interaction_matrix,
        binary_interaction_variable = binary_interaction_variable,
        task_baseline = task_baseline
      )
      return(fit_object)
    },
    .predict = function(task) {
      append_interaction_matrix <- self$fit_object$append_interaction_matrix
      binary_interaction_variable <- self$fit_object$binary_interaction_variable
      beta <- self$fit_object$coefficients
      lrnr_baseline <- self$fit_object$lrnr_baseline
      covariates <- self$fit_object$covariates
      family <- self$fit_object$family
      formula <- self$fit_object$formula
      interaction_variable <- self$params$interaction_variable

      if (is.null(interaction_variable)) {
        A <- rep(1, task$nrow)
      } else {
        A <- unlist(task$get_data(, interaction_variable))
      }
      V <- model.matrix(formula, task$data)
      colnames(V) <- paste0("V", 1:ncol(V))

      if (!append_interaction_matrix && binary_interaction_variable) {
        task_baseline <- task$next_in_chain(covariates = covariates)
        Q0 <- lrnr_baseline$predict(task_baseline)
      } else {
        if (append_interaction_matrix) {
          X0 <- cbind(task$X[, covariates, with = F], 0 * V)
        } else {
          X0 <- cbind(task$X[, covariates, with = F], 0)
        }
        column_names <- task$add_columns(X0)
        task_baseline0 <- task$next_in_chain(
          covariates = colnames(X0), column_names = column_names
        )
        Q0 <- lrnr_baseline$predict(task_baseline0)
      }
      Q0 <- as.vector(Q0)
      Q1 <- as.vector(family$linkinv(family$linkfun(Q0) + V %*% beta))
      Q <- as.vector(family$linkinv(family$linkfun(Q0) + A * V %*% beta))
      if (self$params$return_matrix_predictions && binary_interaction_variable) {
        predictions <- cbind(Q0, Q1, Q)
        colnames(predictions) <- c("A=0", "A=1", "A")
        predictions <- pack_predictions(predictions)
      } else {
        predictions <- as.numeric(Q)
      }
      return(predictions)
    }
  )
)
