#' both ways step regression
#'
#' This learner does step regression
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
#'   \item{\code{direction = "both"}}{Direction of search}
#'   \item{\code{k=2}}{Penalty on complexity, default is AIC}
#'   \item{\code{...}}{Other parameters passed to
#'     \code{\link[MASS]{stepAIC}}.}
#' }
#'
#' @template common_parameters
#
Lrnr_step <- R6Class(
  classname = "Lrnr_step", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(direction = "both", k = 2, ...) {
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
      
      # As it turns out, this setting is what causes offset use
      if (task$has_node("offset")) {
        args$offset <- task$offset
        g0 <- glm(Y ~ 1, data = data, offset = task$offset, family = binomial())
      } else {
        g0 <- glm(Y ~ 1, data = data, family = binomial())
      }
      
      Xmat <- as.matrix(task$X)
      if (is.integer(Xmat)) {
        Xmat[, 1] <- as.numeric(Xmat[, 1])
      }
      Y <- outcome_type$format(task$Y)
      if (outcome_type$type == "categorical") {
        Y <- as.numeric(Y) - 1
      }
      
      upper <- formula(paste("~", paste(colnames(Xmat), collapse = "+")))
      lower <- formula("~1")
      
      args$data <- as.data.frame(cbind(Xmat, Y))

      args$formula = formula("Y~.")  
      args$ctrl <- glm.control(trace = FALSE)
      SuppressGivenWarnings({
        fit_object = MASS::stepAIC(g0, scope = list(upper = upper, lower = lower), 
                                 direction = "both", k = 2, trace = 0, 
                                 steps = 30)
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
        
        # handling the ordering of coefs and cols in X. step coeffs have intercept
        # first then ordered by selection
        coefs <- private$.fit_object$coef
        names(coefs)[1] = "intercept"
        coefs = coefs[order(names(coefs))]
        names(coefs) = names(coefs)[order(names(coefs))]
        
        if (!all(is.na(coef))) {
          eta <- as.matrix(X[
            , names(coefs), drop = FALSE,
            with = FALSE
            ]) %*% coefs
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
