#' Logic Regression
#'
#' This learner provides fitting procedures for logic regression using
#' the \pkg{LogicReg} package \code{\link[LogicReg]{logreg}} function.
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
#'   - \code{type}: Integer (0-5) indicating which scoring function (type of model) to fit.
#'   (1) classification, (2) regression, (3) logistic regression,
#'   (4) Cox proportional hazards model, (5) exponential survival model,
#'   (0) own scoring function, see LogicReg for more details.
#'   - \code{select}: Integer (0-7) indicating which model selection to use. 
#'   (1) fit a single model, (2) fit multiple, (3) cross-validation, 
#'   (4) null-model permutation test, (5) conditional permutation test, 
#'   (6) a greedy stepwise algorithm, (7) Monte Carlo Logic Regression (using MCMC). 
#'   - \code{ntrees}: number of logic trees to be fit. 
#'   one number for select (1), (4), (6), or (7). and a range (e.g. c(ntreeslow,ntreeshigh))
#'   for other select options.
#'   - \code{penalty}: specifying the penalty parameter allows you to penalize the score of larger models.
#'   only relevant when select=1.
#'   - \code{anneal.control}: simulated annealing parameters, consult \code{\link[LogicReg]{logreg.anneal.control} for tuning.
#'   - \code{...}: Other parameters passed to \code{\link[LogicReg]{logreg}}}.
#'
#' @examples
#' data(cpp_imputed)
#' covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
#' task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")
#'
#' # simple, main-terms GLM
#' lrnr_glm <- make_learner(Lrnr_glm)
#' glm_fit <- lrnr_glm$train(task)
#' glm_preds <- glm_fit$predict()
#'
#' # We can include interaction terms by 'piping' them into this learner.
#' # Note that both main terms and the specified interactions will be included
#' # in the regression model.
#' interaction <- list(c("apgar1", "parity"))
#' lrnr_interaction <- Lrnr_define_interactions$new(interactions = interaction)
#' lrnr_glm_w_interaction <- make_learner(Pipeline, lrnr_interaction, lrnr_glm)
#' fit <- lrnr_glm_w_interaction$train(task)
#' coefs <- coef(fit$learner_fits$Lrnr_glm_TRUE)
Lrnr_logic <- R6Class(
  classname = "Lrnr_logic",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(type=NULL, select=1, ntrees=2, 
                          penalty=2, anneal.control=NULL,
                          anneal.control=NULL, ...) {
      super$initialize(params = args_to_list(), ...)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial", "weights"),
    .train = function(task) {
      args <- self$params
      ## stop if X is not useful!
      if(any(apply(task$X, 2, function(x)length(unique(x))>2))){
        stop("All predictors in task$X must be binary!")
      }
      
      outcome_type <- self$get_outcome_type(task)
      # specify data
      args$resp <- outcome_type$format(task$Y)
      args$bin <- task$X
      
      if (task$has_node("weights")) {
        args$wgt <- task$weights
      }
      
      if(is.null(args$type)){
        if (outcome_type$type == "continuous") {
          args$type <- 2
        } else if (outcome_type$type == "binomial") {
          args$type <- 3
        } else {
          stop(sprintf("Outcome type of %s is not supported", outcome_type$type))
        }
      }
      
      if(is.null(args$anneal.control)){
        warning("Package maintainers mention that their default values are OK, but not great, and recommend investing time in learning how to set the parameters")
      }
      
      fit_object <- call_with_args(LogicReg::logreg, args)
      
      return(fit_object)
    },
    .predict = function(task) {
      verbose <- getOption("sl3.verbose")
      predictions <- stats::predict(
        private$.fit_object,
        newbin = task$X
      )
      return(predictions)
    },
    .required_packages = c("LogicReg")
  )
)
