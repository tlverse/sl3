#' Univariate GARCH Models
#'
#' This learner supports autoregressive fractionally integrated moving average and
#' various flavors of generalized autoregressive conditional heteroskedasticity models for univariate time-series.
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return \code{\link{Lrnr_base}} object with methods for training and prediction
#' @format \code{\link{R6Class}} object.
#' @field variance.model List containing the variance model specification. This includes model, GARCH order, submodel, external regressors and variance tageting. Refer to \code{ugarchspec} for more information.
#' @field mean.model List containing the mean model specification. This includes ARMA model, whether the mean should be included, and external regressors among others. Refer to \code{ugarchspec} for more information.
#' @field distribution.model Conditional density to use for the innovations.
#' @field start.pars List of staring parameters for the optimization routine.
#' @field fixed.pars List of parameters which are to be kept fixed during the optimization.
#' @field n.ahead The forecast horizon.
#'
#' @family Learners
#'
Lrnr_rugarch <- R6Class(classname = "Lrnr_rugarch", inherit = Lrnr_base, portable = TRUE, class = TRUE,
                        public = list(
                          initialize = function(variance.model=list(model = "sGARCH", garchOrder = c(1, 1),
                                                                    submodel = NULL, external.regressors = NULL, variance.targeting = FALSE),
                                                
                                                mean.model=list(armaOrder = c(1, 1), include.mean = TRUE, archm = FALSE,
                                                                archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE),
                                                
                                                distribution.model = "norm",
                                                start.pars = list(),
                                                fixed.pars = list(),
                                                n.ahead=NULL,
                                                ...) {
                            
                            params <- args_to_list()
                            super$initialize(params = params, ...)
                          }
                        ),
                        private = list(
                          .properties = c("timeseries", "continuous"),
                          .train = function(task) {
                            args <- self$params
                            
                            #Support for a single time-series
                            spec_object<-call_with_args(rugarch::ugarchspec, args)
                            
                            #Perhaps might not want to store all the info. TO DO
                            fit_object<-rugarch::ugarchfit(spec_object, task$X)
                            
                            return(fit_object)
                            
                          },
                          
                          #Only simple forecast, do not implement CV based forecast here
                          .predict = function(task = NULL) {
                            
                            params <- self$params
                            n.ahead <- params[["n.ahead"]]
                            
                            if(is.null(n.ahead)){
                              n.ahead=nrow(task$X)
                            }
                            
                            #Give the same output as glm
                            predictions <- rugarch::ugarchforecast(private$.fit_object, data=task$X, n.ahead=n.ahead)
                            predictions<-as.numeric(predictions@forecast$seriesFor)
                            predictions <- structure(predictions, names=1:n.ahead)
                            
                            return(predictions)
                            
                          },
                          .required_packages=c("rugarch")
                        ), 
)


