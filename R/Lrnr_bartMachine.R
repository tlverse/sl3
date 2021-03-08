#' BART Machine Learner
#'
#' This learner implements Bayesian Additive Regression Trees, using the
#' \code{bartMachine} package.
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
#' @return Learner object with methods for training and prediction. See
#'   \code{\link{Lrnr_base}} for documentation on learners.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @family Learners
#'
#' @section Parameters:
#' \describe{
#'   \item{\code{...}}{Parameters passed to 
#'   \code{\link[bartMachine]{bartMachine}} and 
#'   \code{\link[bartMachine]{build_bart_machine}}. See it's documentation for 
#'   details.}
#' }
#'
Lrnr_bartMachine <- R6Class(
  classname = "Lrnr_bartMachine",
  inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(...) {
      
      if(is.null(getOption("java.parameters"))){
        warning(
          "User did not specify Java RAM option, and this learner often fails",
          " with the default RAM of 500MB,\n",
          "so setting that now as `options(java.parameters = '-Xmx2500m')`.\n\n",
          "Note that Xmx parameter's upper limit is system dependent \n",
          "(e.g., 32bit Windows will fail to work with anything much larger",
          "than 1500m), \n",
          "so ideally this option should be specified by the user."
        )
        options(java.parameters = '-Xmx2500m')
      }
      
      super$initialize(params = args_to_list(), ...)
    }
  ),

  private = list(
    .properties = c("continuous", "binomial"),

    .train = function(task) {
      args <- self$params
      
      # get verbosity
      if (is.null(args$verbose)) {
        args$verbose <- getOption("sl3.verbose")
      }
      
      # specify data
      args$X <- as.data.frame(task$X)
      outcome_type <- self$get_outcome_type(task)
      if (outcome_type$type == "categorical") {
        stop("Unsupported outcome type for Lrnr_bartMachine")
      }
      args$y <- outcome_type$format(task$Y)

      fit_object <- call_with_args(bartMachine::bartMachine, args)

      return(fit_object)
    },

    .predict = function(task) {
      predictions <- stats::predict(
        private$.fit_object, new_data = data.frame(task$X)
      )
      return(predictions)
    },
    .required_packages = c("rJava", "bartMachine")
  )
)
