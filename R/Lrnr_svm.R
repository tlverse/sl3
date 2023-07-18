#' Support Vector Machines
#'
#' This learner provides fitting procedures for support vector machines, using
#' the routines from \pkg{e1071} (described in \insertCite{e1071;textual}{sl3}
#' and \insertCite{libsvm;textual}{sl3}, the core library to which \pkg{e1071}
#' is an interface) through a call to the function \code{\link[e1071]{svm}}.
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
#'   - \code{scale = TRUE}: A logical vector indicating the variables to be
#'       scaled. For a detailed description, please consult the documentation
#'       for \code{\link[e1071]{svm}}.
#'   - \code{type = NULL}: SVMs can be used as a classification machine, as a
#'       a regression machine, or for novelty detection. Depending of whether
#'       the outcome is a factor or not, the default setting for this argument
#'       is "C-classification" or "eps-regression", respectively. This may be
#'       overwritten by setting an explicit value. For a full set of options,
#'       please consult the documentation for \code{\link[e1071]{svm}}.
#'   - \code{kernel = "radial"}: The kernel used in training and predicting.
#'       You may consider changing some of the optional parameters, depending
#'       on the kernel type. Kernel options include: "linear", "polynomial",
#'       "radial" (the default), "sigmoid". For a detailed description, consult
#'       the documentation for \code{\link[e1071]{svm}}.
#'   - \code{fitted = TRUE}: Logical indicating whether the fitted values
#'       should be computed and included in the model fit object or not.
#'   - \code{probability = FALSE}: Logical indicating whether the model should
#'       allow for probability predictions.
#'   - \code{...}: Other parameters passed to \code{\link[e1071]{svm}}. See its
#'       documentation for details.
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' data(mtcars)
#' # create task for prediction
#' mtcars_task <- sl3_Task$new(
#'   data = mtcars,
#'   covariates = c(
#'     "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
#'     "gear", "carb"
#'   ),
#'   outcome = "mpg"
#' )
#' # initialization, training, and prediction with the defaults
#' svm_lrnr <- Lrnr_svm$new()
#' svm_fit <- svm_lrnr$train(mtcars_task)
#' svm_preds <- svm_fit$predict()
Lrnr_svm <- R6Class(
  classname = "Lrnr_svm", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(scale = TRUE,
                          type = NULL,
                          kernel = "radial",
                          fitted = TRUE,
                          probability = FALSE,
                          ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial", "categorical"),
    .train = function(task) {
      args <- self$params

      # set SVM type based on detected outcome family
      outcome_type <- self$get_outcome_type(task)
      if (is.null(args$type)) {
        if (outcome_type$type %in% c("continuous", "quasibinomial")) {
          args$type <- "eps-regression"
        } else if (outcome_type$type %in% c("binomial", "categorical")) {
          args$type <- "C-classification"
          args$probability <- TRUE
        } else {
          stop("Detected outcome type is incompatible with Lrnr_svm.")
        }
      }

      # add task data to the argument list
      args$x <- as.matrix(task$X)
      args$y <- outcome_type$format(task$Y)

      if (task$has_node("weights")) {
        # e1071's SVM implementation does not support observation-level weights
        # NOTE: see, e.g., https://cran.r-project.org/web/packages/WeightSVM/
        args$weights <- task$weights
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset
      }

      # NOTE: SVM's formals is essentially empty, hence use of keep_all
      fit_object <- call_with_args(e1071::svm, args, keep_all = TRUE)
      return(fit_object)
    },
    .predict = function(task) {
      outcome_type <- private$.training_outcome_type$type
      predictions <- stats::predict(
        private$.fit_object,
        newdata = task$X,
        probability = outcome_type %in% c("binomial", "categorical")
      )

      if (outcome_type %in% c("binomial", "categorical")) {
        predictions <- attr(predictions, "probabilities")
        if (outcome_type == "categorical") {
          predictions <- pack_predictions(predictions)
        } else if (outcome_type == "binomial") {
          predictions <- as.numeric(predictions[, 2])
        }
      } else {
        predictions <- as.numeric(predictions)
      }
      return(predictions)
    },
    .required_packages = c("e1071")
  ),
)
