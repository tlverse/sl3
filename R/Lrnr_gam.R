#' Generalized Additive Models
#'
#' This learner provides fitting procedures for generalized additive models 
#' using \code{\link[mgcv]{gam}}.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stats predict family
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
#'   \item{\code{formula}}{An optional argument specifying the formula of GAM. 
#'   Input type can be formula or string, or a list of them. If not specified, 
#'   continuous covariates will be smoothen with the smooth terms represented 
#'   using `penalized thin plate regression splines'. For a detailed 
#'   description, please consult the documentation for \code{\link[mgcv]{gam}}.}
#'   
#'   \item{\code{family}}{An optional argument specifying the family of GAM. See 
#'   \code{\link{family}} and \code{\link[mgcv]{family.mgcv}} for a list of 
#'   available families. If not specified, it will be inferred depending on the 
#'   type of the outcome. For now the GAM supports binomial and gaussian if 
#'   \code{formula} is not specified. For a detailed description, please consult 
#'   the documentation for \code{\link[mgcv]{gam}}.}
#'   
#'   \item{\code{method}}{An optional argument specifying the method for 
#'   smoothing parameter selection criterion. Default is set to GCV. For a 
#'   detailed description, please consult the documentation for 
#'   \code{\link[mgcv]{gam}}.}
#'   
#'   \item{\code{...}}{Other parameters passed to \code{\link[mgcv]{gam}}.}
#' }
#'
#' @template common_parameters
#
Lrnr_gam <- R6Class(
  classname = "Lrnr_gam", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(formula = NULL,
                          family = NULL,
                          method = "GCV.Cp",
                          ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  
  private = list(
    .properties = c("continuous", "binomial", "categorical"),
    
    .train = function(task) {
      # load args
      args <- self$params
      # process args
      ## data
      outcome_type <- self$get_outcome_type(task)
      Y <- data.frame(outcome_type$format(task$Y))
      colnames(Y) <- task$nodes$outcome
      args$data <- cbind(task$X, Y)
      ## family
      if (is.null(args$family)) {
        if (outcome_type$type == "continuous") {
          args$family <- gaussian
        } else if (outcome_type$type == "binomial") {
          args$family <- binomial
        } else if (outcome_type$type == "categorical") {
          # TODO: implement categorical? 
          ##      (have to specify (#{categories} - 1) 
          ##       linear predictors in formula)
          stop("Categorical outcome is unsupported in Lrnr_gam for now.")
        } else {
          stop("Specified outcome type is unsupported in Lrnr_gam.")
        }
      }
      ## formula
      if (is.null(args$formula)) {
        covars_type = lapply(task$X, 
                             function(iter) variable_type(x = iter)$type)
        i_discrete = covars_type %in% c("binomial", "categorical")
        X_discrete = task$X[, ..i_discrete]
        i_continuous = covars_type == "continuous"
        X_continuous = task$X[, ..i_continuous]
        "y ~ s(x1) + s(x2) + x3"
        X_smooth = sapply(colnames(X_continuous), 
                          function(iter) paste0("s(", iter, ")"))
        if (length(X_continuous) > 0 & length(X_continuous) > 0) {
          args$formula = as.formula(paste(c(colnames(Y),
                                            paste(c(X_smooth, 
                                                    colnames(X_discrete)),
                                                  collapse = " + ")),
                                          collapse = " ~ "))
        } else if (length(X_continuous) > 0) {
          args$formula = as.formula(paste(c(colnames(Y),
                                            paste(X_smooth,
                                                  collapse = " + ")),
                                          collapse = " ~ "))
        } else if (length(X_discrete) > 0) {
          args$formula = as.formula(paste(c(colnames(Y),
                                            paste(colnames(X_discrete),
                                                  collapse = " + ")),
                                          collapse = " ~ "))
        } else {
          stop("Specified covariates types are unsupported in Lrnr_gam.")
        }
      } else if (is.list(args$formula)) {
        args$formula = lapply(args$formula, as.formula)
      } else {
        args$formula = as.formula(args$formula)
      }
      # fit
      fit_object <- call_with_args(mgcv::gam, args)
      return(fit_object)
    },
    
    .predict = function(task) {
      # get predictions
      predictions <- stats::predict(
        private$.fit_object,
        newdata = task$X
      )
      predictions <- as.numeric(predictions)
      return(predictions)
    },
    
    .required_packages = c("mgcv")
  )
)
