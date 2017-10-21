#' Grid Search Models with h2o
#'
#' This learner provides facilities for fitting various types of models with
#' support for grid search over the hyperparameter space of such models, using
#' an interface to the H2O platform. For details on the procedures available and
#' any limitations, consult the documentation of the \code{h2o} package.
#'
#' @docType class
#'
#' @keywords data
#'
#' @return \code{\link{Lrnr_base}} object with methods for training and
#'  prediction.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @importFrom R6 R6Class
#' @importFrom utils getFromNamespace
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @rdname Lrnr_h2o_grid
#'
#' @name Lrnr_h2o_grid
#'
#' @export
#
Lrnr_h2o_grid <- R6Class(classname = "Lrnr_h2o_grid", inherit = Lrnr_base,
                         portable = TRUE, class = TRUE,
  public = list(
    initialize = function(seed = 1,
                    distribution = NULL,
                    intercept = TRUE,
                    standardize = TRUE,
                    lambda = 0L,
                    max_iterations = 100,
                    ignore_const_cols = FALSE,
                    missing_values_handling = "Skip", ...) {
      super$initialize(params = args_to_list(), ...)
    }
  ),
  private = list(
    .classify = FALSE,
    .return_prediction_as_vector = TRUE,
    .properties = c("continuous", "binomial", "categorical", "weights", "offset"),
    .train = function(task) {
      verbose <- getOption("sl3.verbose")
      args <- self$params

      if (verbose) {
        h2o::h2o.show_progress()
      } else {
        h2o::h2o.no_progress()
      }

      connectH2O <- try(h2o::h2o.getConnection(), silent = TRUE)
      if (inherits(connectH2O, "try-error")) {
        stop("No active H2O cluster found, please initiate h2o cluster first by running 'h2o::h2o.init()'")
      }

      outcome_type <- self$get_outcome_type(task)
      
      
      if(is.null(args$family)){
        args$family <- outcome_type$glm_family()
      }
      
      if(inherits(args$family,"family")){
        args$family <- args$family$family
      }
      
      args$distribution <- args$family

      h2o_data <- define_h2o_X(task, outcome_type)


      args$x <- task$nodes$covariates
      args$y <- task$nodes$outcome
      args$training_frame <- h2o_data

      if(task$has_node("weights")){
        args$weights_column <- task$nodes$weights
      }

      if(task$has_node("offset")){
        args$offset_column <- task$nodes$offset
      }


      # keep_cross_validation_predictions = TRUE, keep_cross_validation_fold_assignment = TRUE,
      algorithm <- args$algorithm

      if (is.null(algorithm)) {
        stop("must specify the 'algorithm' name when running 'h2o.grid'")
      }
      if (!is.character(algorithm)) {
        stop("'algorithm' must be a string naming the 'algorithm' for 'h2o.grid'")
      }

      if (algorithm %in% "pca") {
        algo_fun_name <- "h2o.prcomp"
      } else if (algorithm %in% "naivebayes") {
        algo_fun_name <- "h2o.naiveBayes"
      } else {
        algo_fun_name <- paste0("h2o.", algorithm)
      }

      algo_fun <- utils::getFromNamespace(algo_fun_name, ns = 'h2o')
      # Keep only the relevant algorithm args in mainArgs list:
      mainArgs <- keep_only_fun_args(args, fun = algo_fun)

      #add back in args to h2o.grid
      mainArgs[["algorithm"]] <- algorithm
      mainArgs[["search_criteria"]] <- args[["search_criteria"]]
      mainArgs[["hyper_params"]] <- args[["hyper_params"]]

      # Remove any args from mainArgs that also appear in hyper_params:
      common_hyper_args <- intersect(names(mainArgs),
                                     names(mainArgs$hyper_params))
      if (length(common_hyper_args) > 0) {
        mainArgs <- mainArgs[!(names(mainArgs) %in% common_hyper_args)]
      }

      if (("lambda_search" %in% names(mainArgs))) {
        if (mainArgs[["lambda_search"]]) {
          mainArgs[["lambda"]] <- NULL
        }
      }


      # todo: make this play nice with outcome_type
      ## if dealing with classification problem then need to set outcome as factor:
      classify <- private$.classify || (!is.null(mainArgs[["distribution"]]) &&
                                        (mainArgs[["distribution"]] %in% "bernoulli"))
      outvar <- task$nodes$outcome
      outfactors <- as.vector(h2o::h2o.unique(mainArgs[["training_frame"]][, outvar]))
      if (classify) {
        mainArgs[["training_frame"]][, outvar] <- h2o::as.factor(mainArgs[["training_frame"]][, outvar])
      }

      if (verbose) {
        print(paste("running h2o.grid with algorithm: ", algorithm))
      }
      fit_object <- try(do.call(h2o::h2o.grid, mainArgs), silent = FALSE)
      if (inherits(fit_object, "try-error")) {
        if (verbose) {
          message("h2o.grid failed")
        }
        return(fit_object)
      }
      fit_object <- h2o::h2o.getGrid(fit_object@grid_id)

      if (verbose) {
        print(paste0("h2o grid object ID: ", fit_object@grid_id))
        print("h2o grid models: "); print(fit_object)
      }

      h2o::h2o.show_progress()
      return(fit_object)
    },

    # TODO: write S3 predict method for object "H2OGrid" or just write a new
    # custom (not S3 method)
    .predict = function(task = NULL) {
      verbose <- getOption("sl3.verbose")
      if (verbose) {
        h2o::h2o.show_progress()
      } else {
        h2o::h2o.no_progress()
      }
      X <- define_h2o_X(task)

      ## Put all model fits from the grid into a single list for easier access:
      modelfits_all <- lapply(private$.fit_object@model_ids,
                              function(model_id) h2o::h2o.getModel(model_id))
      modelfits_all[[1]]@model_id

      pAout_h2o <- NULL
      for (idx in seq_along(modelfits_all)) {
        pred <- h2o::h2o.predict(modelfits_all[[idx]], X)
        if (private$.return_prediction_as_vector) {
          pred <- pred[, ncol(pred)]
        }
        pAout_h2o <- h2o::h2o.cbind(pAout_h2o, pred)
      }
      names(pAout_h2o) <- paste0(names(pAout_h2o), "_",
                                 seq_along(modelfits_all))
      predictions <- data.table::as.data.table(pAout_h2o)
      h2o::h2o.show_progress()
      return(predictions)
    },
    .required_packages = c("h2o")
  )
)

#' Classification Models with h2o
#'
#' @docType class
#'
#' @keywords data
#'
#' @return \code{\link{Lrnr_base}} object with methods for training and
#'  prediction.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @importFrom R6 R6Class
#'
#' @rdname Lrnr_h2o_grid
#'
#' @name Lrnr_h2o_grid
#'
#' @export
#
Lrnr_h2o_classifier <- R6Class(classname = "Lrnr_h2o_classifier",
                               inherit = Lrnr_h2o_grid, portable = TRUE,
                               class = TRUE,
  private = list(
    .classify = TRUE,
    .return_prediction_as_vector = FALSE),
)

#' Mutate Grid Search Models with h2o
#'
#' @docType class
#'
#' @keywords data
#'
#' @return \code{\link{Lrnr_base}} object with methods for training and
#'  prediction.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @importFrom R6 R6Class
#'
#' @rdname Lrnr_h2o_grid
#'
#' @name Lrnr_h2o_grid
#'
#' @export
#
Lrnr_h2o_mutator <- R6Class(classname = "Lrnr_h2o_mutator",
                            inherit = Lrnr_h2o_grid, portable = TRUE,
                            class = TRUE,
  private = list(
    .classify = FALSE,
    .return_prediction_as_vector = FALSE),
)

