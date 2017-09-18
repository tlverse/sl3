#' Nonlinear Time Series Analysis
#'
#' This learner supports various forms of nonlinear autoregression, including additive AR, neural nets, SETAR and LSTAR models, threshold VAR and VECM.
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return \code{\link{Lrnr_base}} object with methods for training and prediction
#' @format \code{\link{R6Class}} object.
#' @field learner Available built-in time series models. Currently available are listed by with availableModels() function.
#' @field m embedding dimension.
#' @field d time delay.
#' @field include Type of deterministic regressors to include.
#' @field type Whether the variable is taken is level, difference or a mix as in the ADF test.
#' @field mL autoregressive order for low regime.
#' @field mH autoregressive order for high regime.
#' @field mM autoregressive order for middle regime.
#' @field thDelay Time delay for the threshold variable.
#' @field common Indicates which elements are common to all regimes.
#' @field ML vector of lags for order for low. 
#' @field MM vector of lags for order for middle.
#' @field MH vector of lags for order for high.
#' @field nthresh Threshold of the model.
#' @field trim trimming parameter indicating the minimal percentage of observations in each regime.
#' @field sig significance level for the tests to select the number of regimes.
#' @field control further arguments to be passed as control list to optim.
#' @field r Number of cointegrating relationships.
#' @field model Model to estimate. Choices: VAR/VECM/TAR/MTAR.
#' @field I For VAR only: whether in the VAR variables are to be taken in levels or as a difference.
#' @field beta For VECM only: imposed cointegrating value. 
#' @field estim Type of estimator for the VECM (two-step approach or Johansen MLE).
#' @field exogen Inclusion of exogenous variables.
#' @field LRinclude Possibility to include in the long-run relationship and the ECT trend.
#' @field commonInter Whether the deterministic regressors are regime specific.
#' @field mTh combination of variables with same lag order for the transition variable.
#' @field gamma prespecified threshold values.
#' @field dummyToBothRegimes Whether the dummy in the one threshold model is applied to each regime. 
#' @field max.iter Number of iterations for the algorithm.
#' @field size number of hidden units in the neural network.
#' @field lag Number of lags to include in each regime.
#' @field n.ahead The forecast horizon.
#' @field ngridBeta number of elements to search for the cointegrating value.
#' @field ngridTh number of elements to search for the threshold value.
#' @field th1 different possibilities to pre-specify an exact value, an interval or a central point for the search of the threshold. 
#' @field th2 different possibilities to pre-specify an exact value or a central point for the search of the second threshold. 
#' @field beta0 Additional regressors to include in the cointegrating relation.
#'
#' @importFrom assertthat assert_that is.count is.flag
#' 
#' @family Learners
#' 
Lrnr_tsDyn <- R6Class(classname = "Lrnr_tsDyn", inherit = Lrnr_base, portable = TRUE, class = TRUE, 
                        public = list(
                          initialize = function(learner=learner,
                                                m=1,
                                                size=1,
                                                lag=1,
                                                d=1,
                                                include= "const",
                                                type= "level",
                                                n.ahead=NULL, 
                                                mL=m,mH=m,mM=NULL,
                                                thDelay=0,
                                                common="none",
                                                ML=seq_len(mL),MM=NULL,MH=seq_len(mH),
                                                nthresh=1,trim=0.15,
                                                sig=0.05, control=list(),
                                                r=1,model="VAR",I="level",beta=NULL,estim="2OLS",
                                                exogen = NULL,LRinclude="none",
                                                commonInter=FALSE,mTh=1,gamma = NULL,
                                                dummyToBothRegimes=TRUE,max.iter=2,
                                                ngridBeta=50,ngridTh=50,
                                                th1=list(exact = NULL, int = c("from","to"), around = "val"),
                                                th2 = list(exact = NULL, int = c("from", "to"),around = "val"),
                                                beta0=0,...) {
                            
                            params <- list(learner=learner,m=m,size=size,lag=lag,d=d,include=include,
                                           type=type,n.ahead=n.ahead,mL=mL,mM=mM,mH=mH,thDelay=thDelay,
                                           common=common,ML=ML,MM=MM,MH=MH,nthresh=nthresh,trim=trim,
                                           sig=sig,control=control,r=r,model=model,I=I,beta=beta,estim=estim,
                                           exogen=exogen,LRinclude=LRinclude,commonInter=commonInter,mTh=mTh,
                                           gamma=gamma,dummyToBothRegimes=dummyToBothRegimes,max.iter=max.iter,
                                           ngridBeta=ngridBeta,ngridTh=ngridTh,th1=th1,th2=th2,
                                           beta0=beta0,...)
                            super$initialize(params = params)
                          }
                        ),
                        private = list(
                          
                          .train = function(task) {
                            
                            params <- self$params
                            
                            learner <- params[["learner"]]
                            m <- params[["m"]]
                            size <- params[["size"]]
                            lag <- params[["lag"]]
                            d <- params[["d"]]
                            include <- params[["include"]]
                            type <- params[["type"]]
                            mL <- params[["mL"]]
                            mM <- params[["mM"]]
                            mH <- params[["mH"]]
                            thDelay <- params[["thDelay"]]
                            common <- params[["common"]]
                            ML <- params[["ML"]]
                            MM <- params[["MM"]]
                            MH <- params[["MH"]]
                            nthresh <- params[["nthresh"]]
                            trim <- params[["trim"]]
                            sig <- params[["sig"]]
                            control <- params[["control"]]
                            r <- params[["r"]]
                            model <- params[["model"]]
                            I <- params[["I"]]
                            beta <- params[["beta"]]
                            estim <- params[["estim"]]
                            exogen <- params[["exogen"]]
                            LRinclude <- params[["LRinclude"]]
                            commonInter <- params[["commonInter"]]
                            mTh <- params[["mTh"]]
                            gamma <- params[["gamma"]]
                            dummyToBothRegimes <- params[["dummyToBothRegimes"]]
                            max.iter <- params[["max.iter"]]
                            ngridBeta <- params[["ngridBeta"]]
                            ngridTh <- params[["ngridTh"]]
                            th1 <- params[["th1"]]
                            th2 <- params[["th2"]]
                            beta0 <- params[["beta0"]]

                            if(learner=="nnetTs"){
                              
                              if (is.character(learner)) {
                                learner <- get(learner, mode = "function", envir = asNamespace("tsDyn"))
                              }
                              
                              
                              fit_object<-learner(task$X, m=m, d=d, size = size, control=control)
                              
                            }else if(learner=="setar"){
                              
                              if (is.character(learner)) {
                                learner <- get(learner, mode = "function", envir = asNamespace("tsDyn"))
                              }
                              
                              if(!model %in% c("TAR", "MTAR")){
                                stop("When trying to fit self exciting threshold autoregressive model, must specify model to be either TAR or MTAR.")
                              }
                              
                              fit_object<-learner(as.matrix(task$X), m=m, d=d, mL=mL,mM=mM,mH=mH,thDelay=thDelay,mTh=mTh,
                                                  common=common,model=model,
                                                  ML=ML,MM=MM,MH=MH,nthresh=nthresh,trim=trim,type=type)
                              
                            }else if(learner=="lstar"){
                              
                              if (is.character(learner)) {
                                learner <- get(learner, mode = "function", envir = asNamespace("tsDyn"))
                              }
                              
                              fit_object<-learner(as.matrix(task$X), m=m, d=d, mL=mL,mH=mH,thDelay=thDelay,control=control)
                                                  
                            }else if(learner=="star"){
                              
                              if (is.character(learner)) {
                                learner <- get(learner, mode = "function", envir = asNamespace("tsDyn"))
                              }
                              
                              fit_object<-learner(as.matrix(task$X), m=m, d=d, thDelay=thDelay,sig=sig,control=control)
                              
                            }else if(learner=="aar"){
                              
                              if (is.character(learner)) {
                                learner <- get(learner, mode = "function", envir = asNamespace("tsDyn"))
                              }
                              
                              fit_object<-learner(task$X, m=m, d=d)
                              
                            }else if(learner=="lineVar"){
                              
                              if (is.character(learner)) {
                                learner <- get(learner, mode = "function", envir = asNamespace("tsDyn"))
                              }
                              
                              if(!model %in% c("VAR", "VECM")){
                                stop("Must specify model to be either VAR or VECM.")
                              }
                              
                              fit_object<-learner(task$X,lag=lag,r=r,include=include,model=model,I=I,
                                                  beta=beta,estim=estim,LRinclude=LRinclude,exogen)
                              
                            }else if(learner=="VECM"){
                              
                              if (is.character(learner)) {
                                learner <- get(learner, mode = "function", envir = asNamespace("tsDyn"))
                              }
                              
                              fit_object<-learner(task$X,lag=lag,r=r,include=include,
                                                  beta=beta,estim=estim,LRinclude=LRinclude,exogen=exogen)
                              
                            }else if(learner=="TVAR"){
                              
                              if (is.character(learner)) {
                                learner <- get(learner, mode = "function", envir = asNamespace("tsDyn"))
                              }
                              
                              if(!model %in% c("TAR", "MTAR")){
                                stop("When trying to fit multivariate Threshold VAR model, must specify model to be either TAR or MTAR.")
                              }
                              
                              fit_object<-learner(task$X,lag=lag,include=include,model=model,commonInter=commonInter,
                                                  nthresh=nthresh,thDelay=thDelay,mTh=mTh,trim=trim,
                                                  dummyToBothRegimes=dummyToBothRegimes,max.iter=max.iter)

                            }else if(learner=="TVECM"){
                              
                              if (is.character(learner)) {
                                learner <- get(learner, mode = "function", envir = asNamespace("tsDyn"))
                              }
                              
                              fit_object<-learner(task$X,lag=lag,nthresh=nthresh,trim=trim,ngridBeta=ngridBeta,
                                                  ngridTh=ngridTh,th1=th1,th2=th2,common=common,include=include,
                                                  dummyToBothRegimes=dummyToBothRegimes,beta0=beta0)
                              
                            }else if(learner=="linear"){
                              
                              if (is.character(learner)) {
                                learner <- get(learner, mode = "function", envir = asNamespace("tsDyn"))
                              }
                              
                              fit_object<-learner(task$X,m=m,d=d,include=include,type=type)
 
                            }
                              
                            return(fit_object)
                            
                          }, 
                          
                        
                          .predict = function(task = NULL) {
                            
                            params <- self$params
                            n.ahead <- params[["n.ahead"]] 
                            learner <- params[["learner"]]
                            
                            if(is.null(n.ahead)){
                              n.ahead=nrow(task$X)
                            }
                            
                            if(learner=="TVAR"){
                              
                              stop("No forecast for multivariate Threshold VAR model implemented.")
                              
                            }else{
                              
                              predictions <- predict(private$.fit_object, n.ahead=n.ahead)
                              
                              #Create output as in glm
                              predictions <- as.numeric(predictions)
                              predictions <- structure(predictions, names=1:n.ahead)
                              
                              return(predictions)
                              
                              
                            }
                            
                          }, 
                          .required_packages=c("tsDyn")
                        ), )



