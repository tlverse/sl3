

#' Class for Task Data
#'
#' A thin wrapper around a \code{data.table} containing the data. Contains metadata about the particular machine learning problem, including which variables are to be used as covariates and outcomes.
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return \code{sl3_Task} object
#' @format \code{\link{R6Class}} object.
#' @field data Underlying representation of the data
#' @field nodes A list indicating which columns of \code{data} have which purpose
#' @field X a data.table containing the covariates
#' @field Y a vector containing the outcomes
#' @field weights a vector containing the observation weights. If weights aren't specified on construction, weights will default to 1
#' @field id a vector containing the observation units. 
#' @section Methods:
#' \describe{
#'   \item{\code{new(data, covariates, outcome, outcome_type, id, weights, folds, nodes)}}{This method is used to create an object of this class. todo: describe inputs and behavior}
#'   \item{\code{next_in_chain(new_X)}}{Generates a copy of this task with the set of covariates redefined. This is mostly to be used internally for \code{\link{Pipeline}}s}
#'   }
#' @importFrom assertthat assert_that is.count is.flag
#' @import data.table
sl3_Task <- R6Class(classname = "sl3_Task",
                       portable = TRUE,
                       class = TRUE,
                       public = list(
                         outcome_type = NULL,       # Values of the binary outcome (Ynode) in observed data where det.Y = TRUE obs are set to NA
                         initialize = function(data, covariates, outcome, outcome_type=NULL, id=NULL, weights=NULL, folds=NULL, nodes=NULL) {
                           assert_that(is.data.frame(data) | is.data.table(data))
                           self$data <- data.table(data) # makes a copy of the input data (shallow)
                           if(is.null(nodes)){
                             nodes <- list(covariates=covariates,
                                           outcome = outcome,
                                           id = id,
                                           weights= weights)
                           } else {
                             # todo: validate node schema
                           }
                           all_nodes=unlist(nodes)

                           #verify nodes are contained in dataset
                           assert_that(all(all_nodes%in%names(data)))

                           private$.nodes = nodes
                           # generate folds
                           # maybe set keys based on fold object if it exists
                           #setkeyv(self$data)
                           #guess outcome type by count unique

                           invisible(self)
                         },
                         next_in_chain=function(new_X){
                           #construct new internal dataset
                           nodes=self$nodes

                           #all but covariates
                           other_node_types=setdiff(names(nodes),"covariates")
                           chain_nodes=nodes[other_node_types]

                           old_nodes=unlist(chain_nodes)
                           old_data=private$.data[,old_nodes,with=F, drop=F]


                           chain_data=cbind(old_data,new_X)
                           chain_nodes$covariates=colnames(new_X)

                           new_task=self$clone()
                           new_task$initialize(chain_data,nodes=chain_nodes)

                           return(new_task)
                         }),
                       active = list(
                         data = function(data) {
                           if (missing(data)) {
                             return(private$.data)
                           } else {
                             assert_that(is.matrix(data) | is.data.table(data))
                             private$.data <- data
                           }
                         },
                         nodes = function(){
                           private$.nodes
                         },
                         X = function(){
                           private$.data[,private$.nodes$covariates, with=FALSE, drop=FALSE]
                         },
                         Y = function(){
                           private$.data[[private$.nodes$outcome]]
                         },
                         weights = function(){
                           weight_node=private$.nodes$weights
                           if(is.null(weight_node)){
                             weights=rep(1,nrow(private$.data))
                           } else {
                             weights=private$.data[[weight_node]]
                           }

                           return(weights)
                         },
                         id = function(){
                           id_node=private$.nodes$id
                           if(is.null(id_node)){
                             ids=seq_len(nrow(private$.data))
                           } else {
                             ids=private$.data[[id_node]]
                           }

                           return(ids)
                         }),
                       private = list(
                         .data = NULL,
                         .nodes = NULL,
                         .X = NULL
                       )
)

#' @export
`[.sl3_Task` <- function(x,i=NULL,j=NULL,...) {
  sl3_Task$new(x$data[i,],nodes=x$nodes)
}

