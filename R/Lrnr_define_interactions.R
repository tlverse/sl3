


#support for converting data frame with raw columns into a model matrix with things like interaction terms and factor indicators. todo: reimplement this without using model.matrix.
#' @importFrom assertthat assert_that is.count is.flag
#' @export
#' @rdname undocumented_learner
Lrnr_define_interactions <- R6Class(classname = "Lrnr_define_interactions",
                             inherit= Lrnr_base,
                             portable = TRUE,
                             class = TRUE,
                             public = list(
                               initialize = function(interactions, ...) {
                                 params=list(interactions=interactions, ...)
                                 super$initialize(params=params, ...)
                               }),
                             private = list(
                               .train = function(task) {
                                 interaction_names <- task$add_interactions(self$params$interactions)
                                 fit_object <- list(interaction_names = interaction_names)
                                 return(fit_object)
                                 
                               },
                               .predict = function(task = NULL) {
                                 stop("This learner should be used for chaining only")
                               },
                               .chain = function(task = NULL) {
                                 if(!identical(task,private$.training_task)){
                                   new_learners=task$add_interactions(self$params$interactions)
                                 }
                                 
                                 covariates_and_interactions <- unique(c(task$nodes$covariates, private$.fit_object$interaction_names))
                                 
                                 return(task$next_in_chain(covariates=covariates_and_interactions))
                               }
                             )
)



