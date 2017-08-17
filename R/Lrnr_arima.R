#ARIMA

#' @importFrom assertthat assert_that is.count is.flag
#' @importFrom forecast auto.arima
#' @importFrom stats arima
#' @export
#' @rdname undocumented_learner
Lrnr_arima <- R6Class(classname = "Lrnr_arima", inherit = Lrnr_base, portable = TRUE, class = TRUE, 
                      public = list(
                        initialize = function(order=NULL, 
                                              seasonal=list(order = c(0L, 0L, 0L), period = NA),
                                              n.ahead=1,
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
                          ts<-ts(t(task$X))
                          
                          if (is.numeric(ord)) {
                            fit_object <- arima(ts[1,], order=ord, seasonal=season)
                          }else{
                            fit_object <- forecast::auto.arima(ts[1,])
                          }
                          
                          return(fit_object)
                          
                        }, 
                        
                        .predict = function(task = NULL) {
                          
                          params <- self$params
                          n.ahead <- params[["n.ahead"]]            
                          predictions <- predict(private$.fit_object, newdata = task$X, type = "response", n.ahead=n.ahead)
                          
                          return(predictions)
                          
                        }
                      ), )


