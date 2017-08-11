#' @importFrom assertthat assert_that is.count is.flag
#' @export
#' @rdname undocumented_learner
h2o_grid_Learner <- R6Class(classname = "h2o_grid_Learner", inherit = Learner, portable = TRUE, class = TRUE, private = list(
  .covariates = NULL,

  .train = function(task) {
    params <- self$params

    if (inherits(connectH2O <- try(h2o::h2o.getConnection(), silent = TRUE), "try-error")) {
        # if (gvars$verbose)
        message("No active connection to an H2O cluster has been detected.
Will now attempt to initialize a local h2o cluster.
In the future, please run `h2o::h2o.init()` prior to model training with h2o.")
        h2o::h2o.init()
    }

    private$.covariates <- task$nodes$covariates
    if ("covariates" %in% names(params)) {
      private$.covariates <- intersect(private$.covariates, params$covariates)
    }

    X <- define_h2o_X(task, private$.covariates, params)

    # if (gvars$verbose) h2o::h2o.show_progress() else h2o::h2o.no_progress()
    mainArgs <- list(x = private$.covariates,
                     y = task$nodes$outcome,
                     training_frame = X,
                     seed = 1,
                     # keep_cross_validation_predictions = TRUE,
                     # keep_cross_validation_fold_assignment = TRUE,
                     family = "gaussian",
                     intercept = TRUE,
                     standardize = TRUE,
                     lambda = 0L,
                     max_iterations = 100,
                     ignore_const_cols = FALSE,
                     missing_values_handling = "Skip"
                    )

    algorithm <- params[["algorithm"]]

    if (is.null(algorithm)) stop("must specify the 'algorithm' name when running 'h2o.grid'")
    if (!is.character(algorithm)) stop("'algorithm' must be a string naming the 'algorithm' for 'h2o.grid'")
    algo_fun_name <- paste0("h2o.", algorithm)

    # if (!'package:h2o' %in% search()) stop("Could not locate h2o among loaded packages. Please run: 'library('h2o')'")
    # if (!exists(algo_fun_name, where='package:h2o', mode='function')) stop("Could not locate the function " %+% algorithm)

    # Is there a fold_column for cross-validation based model scoring?
    # if (!missing(fold_column)) {
    #   if (!is.null(fold_column) && is.character(fold_column) && (fold_column != "")) {
    #     mainArgs[["fold_column"]] <- fold_column
    #     validation_frame <- NULL
    #     mainArgs[["validation_frame"]] <- NULL
    #   }
    # }

    # Is there a validation frame for model scoring?
    # if (!is.null(validation_frame)) mainArgs[["validation_frame"]] <- validation_frame
    # if ("distribution" %in% names(model_contrl) && ("bernoulli" %in% model_contrl[["distribution"]])) {
    #   mainArgs[["training_frame"]][[y]] <- h2o::as.factor(mainArgs[["training_frame"]][[y]])
    #   if (!is.null(mainArgs[["validation_frame"]])) mainArgs[["validation_frame"]][[y]] <- h2o::as.factor(mainArgs[["validation_frame"]][[y]])
    # }

    ## doesn't work if h2o namespace is not loaded:
    # algo_fun <- get0(algo_fun_name, mode = "function", inherits = TRUE)

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

    if (("lambda_search" %in% names(mainArgs)))
      if (mainArgs[["lambda_search"]])
        mainArgs[["lambda"]] <- NULL

    # if (gvars$verbose)
    print(paste("running h2o.grid with algorithm: ", algorithm))
    fit_object <- try(do.call(h2o::h2o.grid, mainArgs), silent = FALSE)
    if (inherits(fit_object, "try-error")) {message("h2o.grid failed"); return(fit_object)}
    fit_object <- h2o::h2o.getGrid(fit_object@grid_id)

    return(fit_object)
  },

  .predict = function(task = NULL) {
    X <- define_h2o_X(task, private$.covariates, self$params)
    browser()
    ##TODO: write S3 predict method for object "H2OGrid" or just write a new custom (not S3 method)
    predictions <- h2o::h2o.predict(private$.fit_object, X)
    if ("p1" %in% colnames(predictions)) {
      predictions <- as.vector(predictions[,"p1"])
    } else {
      predictions <- as.vector(predictions[,"predict"])
    }
    # predictions <- as.data.table(predictions)
    return(predictions)
}
), )
