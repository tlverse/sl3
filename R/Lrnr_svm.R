#' Support Vector Machines
#'
#' This learner uses \code{\link[svm]{svm}} from \code{e1071} to fit a support
#' vector machine (SVM).
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
#'   \code{\link{Lrnr_base}} for documentation on learners.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @family Learners
#'
#' @section Parameters:
#' \describe{
#'   \item{\code{scale = TRUE}}{A logical vector indicating the variables to be
#'     scaled. For a detailed description, please consult the documentation for
#'     \code{\link[e1071]{svm}}.}
#'   \item{\code{type = NULL}}{SVMs can be used as a classification machine, as
#'     a regression machine, or for novelty detection. Depending of whether the
#'     outcome is a factor or not, the default setting for this argument is
#'     "C-classification" or "eps-regression", respectively. This may be
#'     overwritten by setting an explicit value. For a full set of options,
#'    please consult the documentation for \code{\link[e1071]{svm}}.}
#'   \item{\code{kernel = "radial"}}{The kernel used in training and predicting.
#'     You might consider changing some of the optional parameters, depending on
#'     the kernel type. Options for kernels include: "linear", "polynomial",
#'     "radial" (the default), "sigmoid". For a detailed description, please
#'     consult the documentation for \code{\link[e1071]{svm}}.}
#'   \item{\code{fitted = TRUE}}{Logical indicating whether the fitted values
#'     should be computed and included in the model fit object or not
#'     (default: \code{TRUE}).}
#'   \item{\code{probability = FALSE}}{Logical indicating whether the model
#'     should allow for probability predictions (default: \code{FALSE}).}
#'   \item{\code{...}}{Other parameters passed to \code{\link[e1071]{svm}}.
#'     See its documentation for details.}
#' }
#
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
      # this captures all parameters to initialize and saves them as self$params
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("continuous", "binomial", "categorical"),

    # .train takes task data and returns fit object used to generate predictions
    .train = function(task) {
      verbose <- getOption("sl3.verbose")
      # generate an argument list from the parameters that were
      # captured when your learner was initialized.
      # this allows users to pass arguments directly to your ml function
      args <- self$params

      # get outcome variable type
      # preferring learner$params$outcome_type first, then task$outcome_type
      outcome_type <- self$get_outcome_type(task)

      # should pass something on to your learner indicating outcome_type
      # e.g. family or objective
      if (is.null(args$type)) {
        if (outcome_type$type == "continuous") {
          args$type <- "eps-regression"
        } else if (outcome_type$type %in% c("binomial", "categorical")) {
          args$type <- "C-classification"
        } else {
          stop("Specified outcome type is unsupported in Lrnr_svm.")
        }
      }

      # add task data to the argument list
      # what these arguments are called depends on the learner you are wrapping
      args$x <- as.matrix(task$X)
      args$y <- outcome_type$format(task$Y)

      if (task$has_node("weights")) {
        args$weights <- task$weights
      }

      if (task$has_node("offset")) {
        args$offset <- task$offset
      }

      # call a function that fits your algorithm
      # with the argument list you constructed
      fit_object <- call_with_args(e1071::svm, args,
        other_valid = list("type", "y")
      )
      return(fit_object)
    },

    # .predict takes a task and returns predictions from that task
    .predict = function(task) {
      # get predictions
      predictions <- stats::predict(
        private$.fit_object,
        newdata = task$X
      )
      predictions <- as.numeric(predictions)
      return(predictions)
    },
    .required_packages = c("e1071")
  ),
)
