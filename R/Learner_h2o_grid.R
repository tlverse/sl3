#' @importFrom assertthat assert_that is.count is.flag
#' @export
#' @rdname undocumented_learner
h2o_grid_Learner <- R6Class(classname = "h2o_grid_Learner", inherit = Learner, portable = TRUE, class = TRUE, private = list(
  .covariates = NULL,

  .train = function(task) {
    verbose <- getOption("sl3.verbose")
    params <- self$params
    if (verbose) h2o::h2o.show_progress() else h2o::h2o.no_progress()

    if (inherits(connectH2O <- try(h2o::h2o.getConnection(), silent = TRUE), "try-error")) {
      if (verbose) {
        message("No active connection to an H2O cluster has been detected. Will now attempt to initialize a local h2o cluster. In the future, please run `h2o::h2o.init()` prior to model training with h2o.")
      }
      h2o::h2o.init()
    }

    private$.covariates <- task$nodes$covariates
    if ("covariates" %in% names(params)) {
      private$.covariates <- intersect(private$.covariates, params$covariates)
    }

    X <- define_h2o_X(task, private$.covariates, params)

    mainArgs <- list(x = private$.covariates,
                     y = task$nodes$outcome,
                     training_frame = X,
                     seed = 1,
                     family = "gaussian",
                     intercept = TRUE,
                     standardize = TRUE,
                     lambda = 0L,
                     max_iterations = 100,
                     ignore_const_cols = FALSE,
                     missing_values_handling = "Skip"
                    )
    # keep_cross_validation_predictions = TRUE, keep_cross_validation_fold_assignment = TRUE,

    algorithm <- params[["algorithm"]]

    if (is.null(algorithm)) stop("must specify the 'algorithm' name when running 'h2o.grid'")
    if (!is.character(algorithm)) stop("'algorithm' must be a string naming the 'algorithm' for 'h2o.grid'")

    if (algorithm %in% "pca") {
      algo_fun_name <- "h2o.prcomp"
    } else if (algorithm %in% "naivebayes") {
      algo_fun_name <- "h2o.naiveBayes"
    } else {
      algo_fun_name <- paste0("h2o.", algorithm)
    }

    algo_fun <- utils::getFromNamespace(algo_fun_name, ns='h2o')
    # Keep only the relevant args in mainArgs list:
    mainArgs <- keep_only_fun_args(mainArgs, fun = algo_fun)
    # Add user args that pertain to this specific learner:
    mainArgs <- replace_add_user_args(mainArgs, params, fun = algo_fun)
    mainArgs[["algorithm"]] <- algorithm
    mainArgs[["search_criteria"]] <- params[["search_criteria"]]
    mainArgs[["hyper_params"]] <- params[["hyper_params"]]

    # Remove any args from mainArgs that also appear in hyper_params:
    common_hyper_args <- intersect(names(mainArgs), names(mainArgs$hyper_params))
    if(length(common_hyper_args) > 0) mainArgs <- mainArgs[!(names(mainArgs) %in% common_hyper_args)]

    if (("lambda_search" %in% names(mainArgs))) {
      if (mainArgs[["lambda_search"]]) mainArgs[["lambda"]] <- NULL
    }

    if (verbose) print(paste("running h2o.grid with algorithm: ", algorithm))
    fit_object <- try(do.call(h2o::h2o.grid, mainArgs), silent = FALSE)
    if (inherits(fit_object, "try-error")) {
      if (verbose) {message("h2o.grid failed")}
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

  ##todo: write S3 predict method for object "H2OGrid" or just write a new custom (not S3 method)
  .predict = function(task = NULL) {
    verbose <- getOption("sl3.verbose")
    if (verbose) h2o::h2o.show_progress() else h2o::h2o.no_progress()

    X <- define_h2o_X(task, private$.covariates, self$params)

    ## Put all model fits from the grid into a single list for easier access:
    modelfits_all <- lapply(private$.fit_object@model_ids, function(model_id) h2o::h2o.getModel(model_id))
    modelfits_all[[1]]@model_id

    pAout_h2o <- NULL
    for (idx in seq_along(modelfits_all)) {
      pAout_h2o <- h2o::h2o.cbind(
                    pAout_h2o,
                    h2o::h2o.predict(modelfits_all[[idx]], X)
                    )
    }
    names(pAout_h2o) <- paste0(names(pAout_h2o), "_", seq_along(modelfits_all))
    predictions <- as.data.table(pAout_h2o)

    h2o::h2o.show_progress()
    return(predictions)
  }

), )
