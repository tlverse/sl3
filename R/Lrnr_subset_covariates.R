#' Subset Covariate Learner
#' @importFrom assertthat assert_that is.count is.flag
#' @export
#' @rdname undocumented_learner
Lrnr_subset_covariates <- R6Class(classname = "Lrnr_subset_covariates",
                             inherit= Lrnr_base,
                             portable = TRUE,
                             class = TRUE,
                             public = list(
                               initialize = function(...) {
                                 params=list(...)
                                 super$initialize(...)
                               }),
                             private = list(
                               .train = function(task) {
                                 fit_object <- list()
                                 return(fit_object)
                                 
                               },
                               .predict = function(task = NULL) {
                                 #nothing to do here, because we're relying on Lrnr_base to subset covariates
                                 return(task$X)
                               },
                               .chain = function(task = NULL) {
                                 #nothing to do here, because we're relying on Lrnr_base to subset covariates
                                 return(task)
                               }
                             )
)



