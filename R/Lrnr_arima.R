#' Univariate ARIMA Models
#'
#' This learner supports autoregressive integrated moving average model for univariate time-series.
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return \code{\link{Lrnr_base}} object with methods for training and prediction
#' @format \code{\link{R6Class}} object.
#' @field order A specification of the non-seasonal part of the ARIMA model: the three integer components (p, d, q) are the AR order, the degree of differencing, and the MA order.
#' @field seasonal A specification of the seasonal part of the ARIMA model, plus the period (which defaults to frequency(x)). This should be a list with components order and period, but a specification of just a numeric vector of length 3 will be turned into a suitable list with the specification as the order.
#' @field n.ahead The forecast horizon. If not specified, returns forecast of size \code{task$X}.
#' 
#' @importFrom assertthat assert_that is.count is.flag
#' @importFrom forecast auto.arima
#' @importFrom stats arima
#' 
#' @family Learners
#' 

Lrnr_arima <- R6Class(classname = "Lrnr_arima", inherit = Lrnr_base, portable = TRUE, class = TRUE, 
                      public = list(
                        initialize = function(order=NULL, 
                                              seasonal=list(order = c(0L, 0L, 0L), period = NA),
                                              n.ahead=NULL,
                                              ...) {
                          
                          params <- list(order = order, seasonal=seasonal, n.ahead=n.ahead, ...)
                          super$initialize(params = params)
                        }
                      ),
                      private = list(
                        
                        .train = function(task) {
                          params <- self$params
                          ord <- params[["order"]]
                          season <- params[["seasonal"]]
                          
                          #Support for a single time-series
                          if (is.numeric(ord)) {
                            fit_object <- arima(task$X, order=ord, seasonal=season)
                          }else{
                            fit_object <- forecast::auto.arima(task$X)
                          }
                          
                          return(fit_object)
                          
                        }, 
                        
                        .predict = function(task = NULL) {
                          
                          params <- self$params
                          n.ahead <- params[["n.ahead"]] 
                          
                          if(is.null(n.ahead)){
                            n.ahead=nrow(task$X)
                          }
                          
                          predictions <- predict(private$.fit_object, newdata = task$X, type = "response", n.ahead=n.ahead)
                          
                          #Create output as in glm
                          predictions <- as.numeric(predictions$pred)
                          predictions <- structure(predictions, names=1:n.ahead)
                          
                          return(predictions)
                          
                        }
                      ), )


