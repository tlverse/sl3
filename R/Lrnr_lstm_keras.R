#' Long short-term memory Recurrent Neural Network (LSTM) with Keras
#'
#' This learner supports long short-term memory (LSTM) recurrent neural network
#' algorithm. This learner uses the \code{keras} package. Note that all
#' preprocessing, such as differencing and seasonal effects for time series
#' should be addressed before using this learner. Desired lags of the time series
#' should be added as predictors before using the learner.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return \code{\link{Lrnr_base}} object with methods for training and prediction
#' @format \code{\link{R6Class}} object.
#'
#' @field batch_size How many times should the training data be used to train the neural network?
#' @field units Positive integer, dimensionality of the output space.
#' @field dropout Float between 0 and 1. Fraction of the input units to drop.
#' @field recurrent_dropout Float between 0 and 1. Fraction of the units to drop for the linear transformation of the recurrent state.
#' @field activation Activation function to use. If you pass NULL, no activation is applied (ie. "linear" activation: a(x) = x).
#' @field recurrent_activation Activation function to use for the recurrent step.
#' @field recurrent_out Activation function to use for the output step.
#' @field epochs Number of epochs to train the model.
#' @field lr Learning rate.
#' @field layers How many lstm layers. Only allows for 1 or 2.
#'
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @family Learners
#'

Lrnr_lstm_keras <- R6Class(
  classname = "Lrnr_lstm_keras",
  inherit = Lrnr_base,
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(batch_size = 10,
                          units = 32,
                          dropout = 0.2,
                          recurrent_dropout = 0.2,
                          activation = "tanh",
                          recurrent_activation = "hard_sigmoid",
                          activation_out = "linear",
                          epochs = 10,
                          lr = 0.001,
                          layers = 1,
                          callbacks = list(callback_early_stopping(patience = 2)),
                          ...) {
      params <- list(
        batch_size = batch_size, units = units, dropout = dropout,
        recurrent_dropout = recurrent_dropout, activation = activation,
        recurrent_activation = recurrent_activation, epochs = epochs,
        lr = lr, layers = layers, callbacks = callbacks, ...
      )
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("timeseries", "continuous", "binary", "categorical"),
    .train = function(task) {
      args <- self$params

      # Get data
      data <- task$data
      X <- task$X
      Y <- task$Y

      args$x <- array(
        data = as.matrix(X),
        dim = c(nrow(data), 1, ncol(X))
      )
      args$y <- array(
        data = task$Y,
        dim = c(nrow(data), 1)
      )

      if (args$layers == 1) {
        fit_object <- keras_model_sequential() %>%
          layer_lstm(
            units = args$units,
            activation = args$activation,
            recurrent_activation = args$recurrent_activation,
            dropout = args$dropout,
            recurrent_dropout = args$recurrent_dropout,
            input_shape = c(1, ncol(X))
          ) %>%
          layer_dense(
            units = 1,
            activation = args$activation_out
          )
      } else {
        fit_object <- keras_model_sequential() %>%
          layer_lstm(
            units = args$units,
            activation = args$activation,
            recurrent_activation = args$recurrent_activation,
            dropout = args$dropout,
            recurrent_dropout = args$recurrent_dropout,
            input_shape = c(1, ncol(X)),
            return_sequences = TRUE
          ) %>%
          # layer_dropout(rate = 0.5) %>%
          layer_lstm(
            units = args$units,
            activation = args$activation,
            recurrent_activation = args$recurrent_activation,
            dropout = args$dropout,
            recurrent_dropout = args$recurrent_dropout,
            return_sequences = FALSE
          ) %>%
          layer_dense(
            units = 1,
            activation = args$activation_out
          )
      }

      # TO DO: allow for losses to be passed as well
      if (task$outcome_type$type == "continuous") {
        loss <- "mse"
      } else if (task$outcome_type$type == "binary") {
        loss <- "binary_crossentropy"
      } else if (task$outcome_type$type == "categorical") {
        loss <- "categorical_crossentropy"
      }

      fit_object %>% compile(
        optimizer = optimizer_rmsprop(lr = args$lr),
        loss = loss
      )

      fit_object %>% fit(
        x = args$x,
        y = args$y,
        batch_size = args$batch_size,
        epochs = args$epochs,
        callbacks = callbacks,
        shuffle = FALSE
      )

      return(fit_object)
    },

    .predict = function(task = NULL) {
      args <- self$params

      # Get data
      data <- task$data
      X <- task$X

      args$x <- array(
        data = as.matrix(X),
        dim = c(nrow(data), 1, ncol(X))
      )

      model <- private$.fit_object

      predictions <- model %>% predict(args$x,
        batch_size = args$batch_size
      )

      # Create output as in glm
      predictions <- as.numeric(predictions)
      predictions <- structure(predictions, names = seq_along(predictions))

      return(predictions)
    },
    .required_packages = c("keras")
  )
)
