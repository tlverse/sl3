#' @rdname variable_type
#' @export
Variable_Type <- R6Class(classname = "Variable_Type",
                         portable = TRUE,
                         class = TRUE,
                         public = list(
                           initialize = function(type = NULL, levels = NULL, bounds = NULL, x = NULL, pcontinuous = 0.05){
                             if(is.null(type)){
                               if(is.null(x)){
                                 stop("type not specified, and no x from which to infer it")
                               }
                               nunique <- length(unique(x))
                               
                               if(!is.null(ncol(x))){
                                 type <- "multivariate"
                               } else if(nunique==1){
                                 type <- "constant"
                               } else if(nunique==2){
                                 type <- "binomial"
                               } else if((nunique/length(x))<pcontinuous){
                                 type <- "categorical"
                               } else{
                                 type <- "continuous"
                               }
                             }
                             
                             private$.type = type
                             
                             if(type%in%c("binomial","categorical") && is.null(levels)){
                               if(!is.null(x)){
                                 levels=get_levels(x)
                               } else if(type=="binomial"){
                                 levels=c(0,1)
                               } else
                               if(is.null(levels)){
                                 stop(sprintf("levels or x must be specified for %s", type))
                               }
                             }
                             
                             private$.levels = levels
                             
                             if(type=="quasibinomial" && is.null(bounds)){
                                 bounds=c(0,1)
                             }
                             
                             
                             private$.bounds = bounds
                           },
                           print = function(){
                             print(self$type)
                             print(self$levels)
                             print(self$bounds)
                           },
                           glm_family = function(return_object = FALSE){
                             type = self$type
                             family <- switch(type, 
                                              continuous = "gaussian",
                                              binomial = "binomial",
                                              quasibinomial = "quasibinomial",
                                              categorical = "multinomial",
                                              constant = "binomial",
                                              "unknown")
                             
                             if(family=="unknown"){
                               warning("No family for this outcome_type. Defaulting to gaussian")
                               family <- "gaussian"
                             }
                             
                             if(return_object){
                               family_fun <- try({get(family, mode = "function", envir = parent.frame())})
                               
                               if(inherits(family_fun, "try-error")){
                                 stop("Family object requested for family that does not have a generator.\n",
                                      "You're probably using an unsupported learner/outcome_type combination. Specify family manually.")
                               } else {
                                 family <- family_fun()
                               }
                             }
                             return(family)
                           },
                           format = function(x){
                             if(self$type == "binomial"){
                               max_level <- max(self$levels)
                               formatted <- as.numeric(x == max_level)
                             } else if (self$type == "quasibinomial") {
                               if (any(x<0)||any(x>1)) warning("Detected 'Y' outside [0-1] range with 'quasibinomial' outcome_type -- beware, this will break most learners.")
                               formatted <- x
                             } else if (self$type == "categorical"){
                               formatted <- factor(x, levels = self$levels)
                             } else {
                               formatted <- x
                             }
                             
                             return(formatted)
                           }
                         ),
                         active = list(
                           type = function(){
                             return(private$.type)
                           },
                           levels = function(){
                             return(private$.levels)
                           },
                           bounds = function(){
                             return(private$.bounds)
                           }
                           
                         ),
                         private = list(
                           .type = NULL,
                           .levels = NULL,
                           .bounds = NULL
                         )
)

#' Specify variable type
#' @param type A type name
#' @param levels valid levels for discrete types
#' @param bounds bounds for continous variables
#' @param x data to use for inferring type if not specified
#' @param pcontinuous if type inferred, proportion of unique observations above which variable is continuous
#' @export
variable_type <- function(type = NULL, levels = NULL, bounds = NULL, x = NULL, pcontinuous = 0.05){
  return(Variable_Type$new(type = type, levels = levels, bounds = bounds, x = x, pcontinuous = pcontinuous))
}

