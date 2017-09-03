#' Harmonic Regression
#'
#' This learner fits first harmonics in a Fourier expansion to one or more time series.
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return \code{\link{Lrnr_base}} object with methods for training and prediction
#' @format \code{\link{R6Class}} object.
#' @field n.ahead The forecast horizon. If not specified, returns forecast of size task$X.
#' @field K Maximum order(s) of Fourier terms.
#' @field freq the number of observations per unit of time. 
#'
#' @importFrom assertthat assert_that is.count is.flag
#' @importFrom stats arima
#' @importFrom uuid UUIDgenerate
#'
#' @family Learners
#'

Lrnr_HarmonicReg <- R6Class(classname = "Lrnr_HarmonicReg", inherit = Lrnr_base, portable = TRUE, class = TRUE,
                      public = list(
                        initialize = function(K=K,
                                              n.ahead=NULL,
                                              freq=freq,
                                              ...) {
                          
                          params <- list(n.ahead=n.ahead, K=K, freq=freq, ...)
                          super$initialize(params = params, ...)
                        }
                      ),
                      private = list(
                        
                        .train = function(task) {
                          
                          params <- self$params
              
                          K <- params[["K"]]
                          freq <- params[["freq"]]
                          
                          task_ts <- ts(task$X, frequency = freq)
                          
                          if(length(freq) != length(K)){
                            stop("Number of periods does not match number of orders")
                          }else if(any(2*K > freq)){
                            stop("K must be not be greater than period/2")
                          }
                          
                          fit_object <- forecast::tslm(task_ts~forecast::fourier(task_ts, K=K))
                          
                          return(fit_object)
                          
                        },
                        
                        .predict = function(task = NULL) {
                          
                          params <- self$params
                          n.ahead <- params[["n.ahead"]]
                          
                          if(is.null(n.ahead)){
                            n.ahead=nrow(task$X)
                          }
                          
                          task_ts <- ts(task$X, frequency = freq)
                          
                          predictions <- forecast::forecast(private$.fit_object, data.frame(forecast::fourier(task_ts, K=K, h=n.ahead)))
                          
                          #Create output as in glm
                          predictions <- as.numeric(predictions$mean)
                          predictions <- structure(predictions, names=1:n.ahead)
                          
                          return(predictions)
                          
                        }, 
                        .required_packages = c("forecast")
                      ), )

