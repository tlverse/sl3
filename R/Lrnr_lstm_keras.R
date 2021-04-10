#' Long short-term memory Recurrent Neural Network (LSTM) with Keras
#'
#' This learner supports long short-term memory (LSTM) recurrent neural network
#' algorithm. This learner uses the \code{keras} package. Note that all
#' preprocessing, such as differencing and seasonal effects for time series
#' should be addressed before using this learner. Desired lags of the time series
#' should be added as predictors before using the learner.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @export
#'
#' @keywords data
#'
#' @family Learners
#'
#' @return \code{\link{Lrnr_base}} object with methods for training and prediction.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @section Parameters:
#' \describe{
#'  \item{\code{batch_size}}{How many times should the training data be used to
#'  train the neural network?}
#'  \item{\code{units}}{Positive integer, dimensionality of the output space.}
#'  \item{\code{dropout}}{Float between 0 and 1. Fraction of the input units to
#'  drop.}
#'  \item{\code{recurrent_dropout}}{Float between 0 and 1. Fraction of the
#'  units to drop for the linear transformation of the recurrent state.}
#'  \item{\code{activation}}{Activation function to use. If you pass NULL, no
#'  activation is applied (e.g., "linear" activation: \code{a(x) = x}).}
#'  \item{\code{recurrent_activation}}{Activation function to use for the
#'  recurrent step.}
#'  \item{\code{recurrent_out}}{Activation function to use for the output step.}
#'  \item{\code{epochs}}{Number of epochs to train the model.}
#'  \item{\code{lr}}{Learning rate.}
#'  \item{\code{layers}}{How many lstm layers. Only allows for 1 or 2.}
#'  \item{\code{callbacks}}{List of callbacks, which is a set of functions to
#'  be applied at given stages of the training procedure. Default callback
#'  function \code{callback_early_stopping} stops training if the validation
#'  loss does not improve across \code{patience} number of epochs.}
#'  }
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
                          epochs = 100,
                          lr = 0.001,
                          layers = 1,
                          callbacks = list(keras::callback_early_stopping(patience = 10)),
                          ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("timeseries", "continuous", "binomial", "categorical"),
    .train = function(task) {

      # set verbosity
      verbose <- getOption("keras.fit_verbose")
      if (is.null(verbose)) {
        verbose <- as.numeric(getOption("sl3.verbose"))
      }

      # get arguments
      args <- self$params

      # get data
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
      } else if (task$outcome_type$type == "binomial") {
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
        verbose = verbose,
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
