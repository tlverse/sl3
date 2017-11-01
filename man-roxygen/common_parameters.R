##' @section Common Parameters:
##' 
##' Individual learners have their own sets of parameters. Below is a list of shared parameters, implemented by \code{Lrnr_base}, and shared
##' by all learners.
##' 
##' \describe{
##'   \item{\code{covariates}}{A character vector of covariates. The learner will use this to subset the covariates for any specified task}
##'   \item{\code{outcome_type}}{A \code{\link{variable_type}} object used to control the outcome_type used by the learner. Overrides the task outcome_type if specified}
##'   \item{\code{...}}{All other parameters should be handled by the invidual learner classes. See the documentation for the learner class you're instantiating}
##' }
