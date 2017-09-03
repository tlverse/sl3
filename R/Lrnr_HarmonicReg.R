#' Harmonic Regression
#'
#' This learner fits first harmonics in a Fourier expansion to one or more time series.
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return \code{\link{Lrnr_base}} object with methods for training and prediction
#' @format \code{\link{R6Class}} object.
#' @field order A specification of the non-seasonal part of the ARIMA model: the three integer components (p, d, q) are the AR order, the degree of differencing, and the MA order.
#' @field seasonal A specification of the seasonal part of the ARIMA model, plus the period (which defaults to frequency(x)). This should be a list with components order and period, but a specification of just a numeric vector of length 3 will be turned into a suitable list with the specification as the order.
#' @field n.ahead The forecast horizon. If not specified, returns forecast of size task$X.
#' @field K Maximum order(s) of Fourier terms.
#' @field freq the number of observations per unit of time. 
#'
#'
#' @importFrom assertthat assert_that is.count is.flag
#' @importFrom stats arima
#'
#' @family Learners
#'

Lrnr_HarmonicReg <- R6Class(classname = "Lrnr_HarmonicReg", inherit = Lrnr_base, portable = TRUE, class = TRUE,
                      public = list(
                        initialize = function(order=NULL,
                                              seasonal=list(order = c(0L, 0L, 0L), period = NA),
                                              K=K,
                                              n.ahead=NULL,
                                              freq=freq,
                                              ...) {
                          
                          params <- list(order = order, seasonal=seasonal, n.ahead=n.ahead, K=K, freq=freq, ...)
                          super$initialize(params = params, ...)
                        }
                      ),
                      private = list(
                        
                        .train = function(task) {
                          params <- self$params
                          ord <- params[["order"]]
                          season <- params[["seasonal"]]
                          K <- params[["K"]]
                          freq <- params[["freq"]]
                          
                          task_ts <- ts(task$X, frequency = freq)
                          
                          if(length(freq) != length(K)){
                            stop("Number of periods does not match number of orders")
                          }else if(any(2*K > freq)){
                            stop("K must be not be greater than period/2")
                          }

                          if (is.numeric(ord)) {
                            fit_object <- arima(task_ts, xreg=fourier(task_ts, K=K), order=ord, seasonal=season)
                          }else{
                            fit_object <- forecast::auto.arima(task_ts, xreg=fourier(task_ts, K=K))
                          }
                          
                          return(fit_object)
                          
                        },
                        
                        .predict = function(task = NULL) {
                          
                          params <- self$params
                          n.ahead <- params[["n.ahead"]]
                          
                          if(is.null(n.ahead)){
                            n.ahead=nrow(task$X)
                          }
                          
                          task_ts <- ts(task$X, frequency = freq)
                          
                          predictions <- forecast(private$.fit_object, xreg=fourier(task_ts, K=K, h=n.ahead))
                          
                          #Create output as in glm
                          predictions <- as.numeric(predictions$mean)
                          predictions <- structure(predictions, names=1:n.ahead)
                          
                          return(predictions)
                          
                        }, 
                        .required_packages = c("forecast")
                      ), )


