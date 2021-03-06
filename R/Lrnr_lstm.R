#' Long short-term memory Recurrent Neural Network (LSTM)
#'
#' This learner supports long short-term memory (LSTM) recurrent neural network
#' algorithm. This learner uses the \code{kerasR} package, and in order to use
#' it, you will need \code{keras} Python module 2.0.0 or higher. Note that all
#' preprocessing, such as differencing and seasonal effects for time series,
#' should be addressed before using this learner.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return \code{\link{Lrnr_base}} object with methods for training and prediction
#' @format \code{\link{R6Class}} object.
#'
#' @field units Positive integer, dimensionality of the output space.
#' @field loss Name of a loss function used.
#' @field optimizer name of optimizer, or optimizer object.
#' @field batch_size Number of samples per gradient update.
#' @field epochs Number of epochs to train the model.
#' @field window Size of the sliding window input.
#' @field activation The activation function to use.
#' @field dense regular, densely-connected NN layer. Default is 1.
#' @field dropout float between 0 and 1. Fraction of the input units to drop.
#' @field early_stopping logical indicating whether ot not to interrupt training
#'        when the validation loss is not decreasing anymore.
#' @field patience number of epochs with no improvement after which training
#'        will be stopped, only used when early_stopping = TRUE.
#' @field validation_split float between 0 and 1. Fraction of the data to use
#'        as held-out validation data, only used when early_stopping = TRUE.
#'
#'
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @family Learners
#'

Lrnr_lstm <- R6Class(
  classname = "Lrnr_lstm", inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(units = 4,
                          loss = "mean_squared_error",
                          optimizer = "adam",
                          batch_size = 1,
                          epochs = 500,
                          window = 5,
                          activation = "linear",
                          dense = 1,
                          dropout = 0,
                          early_stopping = FALSE,
                          patience = 2,
                          validation_split = 0.2,
                          ...) {
      params <- list(
        units = units, loss = loss, optimizer = optimizer,
        batch_size = batch_size, epochs = epochs, window = window,
        activation = activation, dense = dense, dropout = dropout,
        early_stopping = early_stopping, patience = patience,
        validation_split = validation_split, ...
      )
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("timeseries", "continuous"),
    .train = function(task) {
      args <- self$params

      # Pad with NA:
      data <- c(rep(NA, args$window), task$Y)

      # Convert to keras input shape:
      args$x <- t(data.frame(lapply(
        1:(length(data)[1] - args$window),
        function(x) data[x:(x + args$window - 1)]
      )))
      row.names(args$x) <- NULL

      args$y <- as.numeric(sapply(
        (args$window + 1):(length(data)[1]),
        function(x) data[x]
      ))

      args$x <- kerasR::expand_dims(args$x, axis = 2)
      args$y <- kerasR::expand_dims(args$y, axis = 1)

      num_samples <- dim(args$x)[1] # based on spliting the time series
      num_steps <- dim(args$x)[2] # window
      num_features <- dim(args$x)[3] # features = ts

      # Build the model
      model <- kerasR::Sequential()
      model$add(kerasR::LSTM(units = args$units, input_shape = c(num_steps, num_features)))
      model$add(kerasR::Dropout(rate = args$dropout))
      model$add(kerasR::Dense(args$dense))
      model$add(kerasR::Activation(args$activation))
      kerasR::keras_compile(model, loss = args$loss, optimizer = args$optimizer)

      # Fit the model
      if (args$early_stopping) {
        callbacks <- list(kerasR::EarlyStopping(patience = args$patience))
        kerasR::keras_fit(model, args$x, args$y,
          batch_size = args$batch_size,
          epochs = args$epochs,
          validation_split = args$validation_split,
          callbacks = callbacks
        )
      } else {
        kerasR::keras_fit(model, args$x, args$y,
          batch_size = args$batch_size,
          epochs = args$epochs
        )
      }
      fit_object <- model

      return(fit_object)
    },

    .predict = function(task = NULL) {
      args <- self$params

      # TO DO: Incorporate gap between validation and training here as well?

      # See if there is gap between training and validation:
      # gap <- min(task$folds[[1]]$validation_set)-max(task$folds[[1]]$training_set)

      # Pad with NA:
      data <- c(rep(NA, args$window), task$Y)

      # Convert to keras input shape:
      args$x <- t(data.frame(lapply(
        1:(length(data)[1] - args$window),
        function(x) data[x:(x + args$window - 1)]
      )))
      row.names(args$x) <- NULL
      args$x <- kerasR::expand_dims(args$x, axis = 2)

      predictions <- kerasR::keras_predict(private$.fit_object, args$x,
        batch_size = args$batch_size
      )

      # Create output as in glm
      predictions <- as.numeric(predictions)
      predictions <- structure(predictions, names = seq_along(predictions))

      return(predictions)
    },
    .required_packages = c("kerasR")
  ),
)
