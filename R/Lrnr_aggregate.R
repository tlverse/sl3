#' Data Aggregating Procedures
#'
#' This learner provides data aggregating procedures by 
#' taking the mean of continuous variables and
#' taking the mode of categorical variables.
#' 
#' @docType class
#'
#' @importFrom R6 R6Class
#'
#' @export
#'
#' @keywords data
#'
#' @return Learner object with methods for training and prediction. See
#'  \code{\link{Lrnr_base}} for documentation on learners.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @family Learners
#' 
#' @section Parameters:
#' \describe{
#' 
#' }
#

Lrnr_aggregate <- R6Class(
  classname = "Lrnr_aggregate",
  inherit = Lrnr_base, 
  portable = TRUE, 
  class = TRUE,
  public = list(
    initialize = function(loc = "pre") {
      params <- args_to_list()
      super$initialize(params = params)
    }
  ),
  
  private = list(
    .properties = c("continuous", "binomial", "categorical", "weights", "offset"),
    
    .train = function(task) {
      fit_object <- list()
      return(fit_object)
    },
    
    .predict = function(task) {
      agg_task <- aggregate_task(task)
      if (ncol(agg_task$X) == 1 & 'predictions' %in% names(agg_task$X)){
        # if we are using Lrnr_aggregate after other learners
        # then just output the vector of aggregated predictions
        agg_task$X[[1]]
      }else{
        # if we are using Lrnr_aggregate before other learners
        # then output the data of aggregated covariates
        agg_task$X
      }
    },
    
    # update data
    .chain = function(task) {
      return(aggregate_task(task))
    }
  )
)

# helper function
hp_aggregate <- function(x){
  if (class(x) == "numeric"){
    mean(x)
  }else{
    mode <- as.data.table(x)[, .N, by=x][, x[N == max(N)]][1]
    mode
  }
}

aggregate_task <- function(task){
  
  task_data <- task$data
  covs  <- task$nodes$covariates
  
  df_covs <- task_data[,covs, with = FALSE]
  df_main <- task_data[,!covs, with = FALSE]
  
  # convert categorical to dummy
  df_covs_expand <- dt_expand_factors(df_covs)
  task_data <- cbind(df_main, df_covs_expand)
  
  # aggregate # TBD for now id must be named as 'id'
  agg_data <- task_data[,lapply(.SD, mean),by=list(id)]
  
  # update covs set
  agg_covs <- names(df_covs_expand)
  
  # update outcome_type since it could be changed after aggregation
  outcome <- task$nodes$outcome
  agg_outcome_type <- variable_type(x = agg_data[,..outcome][[1]])
  
  agg_task <- make_sl3_Task(agg_data, 
                            covariates = agg_covs,  
                            outcome = task$nodes$outcome,
                            outcome_type = agg_outcome_type,
                            id = task$nodes$id)
  
  return(agg_task)
}

