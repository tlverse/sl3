#' Exponential Smoothing
#'
#' This learner supports exponential smoothing models. 
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return \code{\link{Lrnr_base}} object with methods for training and prediction
#' @format \code{\link{R6Class}} object.
#' @field model Three-character string identifying method. In all cases, "N"=none, "A"=additive,
#' "M"=multiplicative, and "Z"=automatically selected. The first letter denotes the error type, second
#' letter denotes the trend type, third letter denotes the season type. For example, "ANN" is simple
#' exponential smoothing with additive errors, "MAM" is multiplicative Holt-Winters' methods with 
#' multiplicative errors, etc.  
#' @field damped If TRUE, use a damped trend (either additive or multiplicative). If NULL, both damped and non-damped trends will be tried and the best model (according to the information criterion ic) returned.
#' @field alpha Value of alpha. If NULL, it is estimated.
#' @field beta Value of beta. If NULL, it is estimated.
#' @field gamma Value of gamma. If NULL, it is estimated.
#' @field phi Value of phi. If NULL, it is estimated.
#' @field lambda Box-Cox transformation parameter. Ignored if NULL. When lambda is specified, additive.only is set to TRUE.
#' @field additive.only If TRUE, will only consider additive models.
#' @field biasadj Use adjusted back-transformed mean for Box-Cox transformations.
#' @field lower Lower bounds for the parameters (alpha, beta, gamma, phi)
#' @field upper Upper bounds for the parameters (alpha, beta, gamma, phi)
#' @field opt.crit Optimization criterion.
#' @field nmse Number of steps for average multistep MSE (1<=nmse<=30).
#' @field bounds Type of parameter space to impose: "usual" indicates all parameters must lie between specified lower and upper bounds; "admissible" indicates parameters must lie in the admissible space; "both" (default) takes the intersection of these regions.
#' @field ic Information criterion to be used in model selection.
#' @field restrict If TRUE, models with infinite variance will not be allowed.
#' @field allow.multiplicative.trend If TRUE, models with multiplicative trend are allowed when searching for a model. 
#' @field use.initial.values If TRUE and model is of class "ets", then the initial values in the model are also not re-estimated.
#' @field n.ahead The forecast horizon. If not specified, returns forecast of size \code{task$X}.
#' @field freq the number of observations per unit of time.
#'
#' @importFrom assertthat assert_that is.count is.flag
#'
#' @family Learners
#'

Lrnr_expSmooth <- R6Class(classname = "Lrnr_expSmooth", inherit = Lrnr_base, portable = TRUE, class = TRUE,
                      public = list(
                        initialize = function(model="ZZZ",
                                              damped=NULL,
                                              alpha=NULL,beta=NULL,gamma=NULL,phi=NULL,lambda=NULL,
                                              additive.only=FALSE,biasadj=FALSE,
                                              lower = c(rep(1e-04, 3), 0.8),upper = c(rep(0.9999,3), 0.98),
                                              opt.crit="lik",nmse = 3,bounds="both",ic="aic",restrict = TRUE,
                                              allow.multiplicative.trend = FALSE,use.initial.values = FALSE,
                                              freq=1, ...) {
                          
                          params <- list(model=model, damped=damped, alpha=alpha,
                                         beta=beta, gamma=gamma, phi=phi, lambda=lambda, additive.only=additive.only,
                                         biasadj=biasadj,lower=lower,upper=upper,opt.crit=opt.crit,nmse=nmse,bounds=bounds,
                                         ic=ic,restrict=restrict,allow.multiplicative.trend=allow.multiplicative.trend,
                                         use.initial.values=use.initial.values,freq=freq, ...)
                          super$initialize(params = params, ...)
                        }
                      ),
                      private = list(
                        
                        .train = function(task) {
                          
                          params <- self$params
                          
                          model <- params[["model"]]
                          damped <- params[["damped"]]
                          alpha <- params[["alpha"]]
                          beta <- params[["beta"]]
                          gamma <- params[["gamma"]]
                          phi <- params[["phi"]]
                          lambda <- params[["lambda"]]
                          additive.only <- params[["additive.only"]]
                          biasadj <- params[["biasadj"]]
                          lower <- params[["lower"]]
                          upper <- params[["upper"]]
                          opt.crit <- params[["opt.crit"]]
                          nmse <- params[["nmse"]]  
                          bounds <- params[["bounds"]]
                          ic <- params[["ic"]]
                          restrict <- params[["restrict"]]
                          allow.multiplicative.trend <- params[["allow.multiplicative.trend"]]
                          use.initial.values <- params[["use.initial.values"]]
                          freq <- params[["freq"]]
    
                          task_ts <- ts(task$X, frequency = freq)
                          
                          if (model=="ZZZ") {
                            fit_object <- forecast::ets(task_ts)
                          }else{
                            
                            fit_object <- forecast::ets(task_ts,model=model,damped=damped,alpha=alpha,beta=beta,
                                                     gamma=gamma,phi=phi,additive.only=additive.only,lambda=lambda,
                                                     biasadj=biasadj,lower=lower,upper=upper,opt.crit=opt.crit,nmse=nmse,
                                                     bounds=bounds,ic=ic,restrict=restrict,
                                                     allow.multiplicative.trend=allow.multiplicative.trend,
                                                     use.initial.values=use.initial.values)
                          }
                          
                          return(fit_object)
                          
                        },
                        
                        .predict = function(task = NULL) {
                          
                          params <- self$params
                          n.ahead <- params[["n.ahead"]]

                          if(is.null(n.ahead)){
                            n.ahead=nrow(task$X)
                          }

                          predictions <- forecast::forecast(private$.fit_object,h=n.ahead)
                          
                          #Create output as in glm
                          predictions <- as.numeric(predictions$mean)
                          predictions <- structure(predictions, names=1:n.ahead)
                          
                          return(predictions)
                          
                        }, 
                        .required_packages = c("forecast")
                      ), )


