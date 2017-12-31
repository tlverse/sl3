#' both ways step regression
#'
#' This learner provides fitting procedures for generalized linear models using
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#'
#' @export
#'
#' @keywords data
#'
#' @return Learner object with methods for training and prediction. See
#'  \code{\link{Lrnr_base}} for documentation on learners.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @family Learners
#'
#' @section Parameters:
#' \describe{
#'   \item{\code{...}}{Parameters passed to \code{\link[stats]{glm}}.}
#' }
#'
#' @template common_parameters
#
Lrnr_step <- R6Class(
  classname = "Lrnr_step", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  
  private = list(
    .properties = c("continuous", "binomial", "weights", "offset"),
    
    .train = function(task) {
      args <- self$params
      outcome_type <- self$get_outcome_type(task)
      
      if (is.null(args$family)) {
        args$family <- outcome_type$glm_family(return_object = TRUE)
      }
      family_name <- args$family$family
      linkinv_fun <- args$family$linkinv
      # specify data
      
      if (task$has_node("weights")) {
        args$weights <- task$weights
      }
      
      if (task$has_node("offset")) {
        args$offset <- task$offset
      }

      Xmat <- as.matrix(task$X)
      if (is.integer(Xmat)) {
        Xmat[, 1] <- as.numeric(Xmat[, 1])
      }
      Y <- outcome_type$format(task$Y)
      if (outcome_type$type == "categorical") {
        Y <- as.numeric(Y) - 1
      }
      args$data <- as.data.frame(cbind(Xmat, Y))

      args$formula = formula("Y~.")  
      args$ctrl <- glm.control(trace = FALSE)
      SuppressGivenWarnings({
        fit.glm <- sl3:::call_with_args(stats::glm, args)
        fit_object <- stats::step(fit.glm, direction = "both", trace = 0, k = 2) 
        }, GetWarningsToSuppress())
      
      fit_object$linear.predictors <- NULL
      fit_object$weights <- NULL
      fit_object$prior.weights <- NULL
      fit_object$y <- NULL
      fit_object$residuals <- NULL
      fit_object$fitted.values <- NULL
      fit_object$effects <- NULL
      fit_object$qr <- NULL
      fit_object$linkinv_fun <- linkinv_fun
      
      return(fit_object)
    },
    
    .predict = function(task = NULL) {
      verbose <- getOption("sl3.verbose")
      X <- task$X_intercept
      predictions <- rep.int(NA, nrow(X))
      if (nrow(X) > 0) {
        coef <- private$.fit_object$coef
        coef <- fit_object$coef
        if (!all(is.na(coef))) {
          eta <- as.matrix(X[
            , which(!is.na(coef)), drop = FALSE,
            with = FALSE
            ]) %*% coef[!is.na(coef)]
          if (task$has_node("offset")) {
            eta = eta + task$offset
          }
          predictions <- as.vector(private$.fit_object$linkinv_fun(eta))
        }
      }
      return(predictions)
    }
  )
)
