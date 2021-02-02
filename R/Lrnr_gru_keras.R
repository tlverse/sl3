#' Recurrent Neural Network with Gated Recurrent Unit (GRU) with Keras
#'
#' This learner supports the Recurrent Neural Network (RNN) with
#' Gated Recurrent Unit. This learner leverages the same principle as a LSTM,
#' but it is more streamlined and thus cheaper to run, at the expense of
#' representational power. This learner uses the \code{keras} package. Note that all
#' preprocessing, such as differencing and seasonal effects for time series,
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
#' @field epochs Number of epochs to train the model.
#' @field lr Learning rate.
#'
#'
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @family Learners
#'

Lrnr_gru_keras <- R6Class(
  classname = "Lrnr_gru_keras", inherit = Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(batch_size = 10,
                          units = 32,
                          dropout = 0.2,
                          recurrent_dropout = 0.2,
                          activation = "tanh",
                          recurrent_activation = "hard_sigmoid",
                          epochs = 10,
                          lr = 0.001,
                          ...) {
      params <- list(
        batch_size = batch_size, units = units, dropout = dropout,
        recurrent_dropout = recurrent_dropout, activation = activation,
        recurrent_activation = recurrent_activation, epochs = epochs,
        lr = lr, ...
      )
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("timeseries", "continuous"),
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

      fit_object <- keras_model_sequential() %>%
        layer_gru(
          units = args$units,
          activation = args$activation,
          recurrent_activation = args$recurrent_activation,
          dropout = args$dropout,
          recurrent_dropout = args$recurrent_dropout,
          input_shape = c(1, ncol(X))
        ) %>%
        layer_dense(units = 1)

      # TO DO: allow for losses to be passed as well
      if (task$outcome_type$type == "continuous") {
        loss <- "mse"
      } else if (task$outcome_type$type == "binary") {
        loss <- "binary_crossentropy"
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
