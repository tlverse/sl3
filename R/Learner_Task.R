#' @import data.table

# a dataset with metadata about which columns serve which roles, and helper functions to munge the data as needed for ML

#' @importFrom assertthat assert_that is.count is.flag
#' @export
Learner_Task <- R6Class(classname = "Learner_Task",
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
                         Xmat = function(){
                           if(is.null(private$.Xmat)){
                             private$generate_Xmat()
                           }

                           return(private$.Xmat)
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
                         .X = NULL,
                         .Xmat = NULL,
                         generate_Xmat=function(){
                           private$.Xmat=model.matrix(~.,data = self$X)
                         }
                       )
)

#' @export
`[.Learner_Task` <- function(x,i=NULL,j=NULL,...) {
  #todo: if we decide to keep using xmat, also subset here rather than regenerating for subset
  Learner_Task$new(x$data[i,],nodes=x$nodes)
}

