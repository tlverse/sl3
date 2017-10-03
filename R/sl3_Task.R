

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
#' @importFrom uuid UUIDgenerate
#' @import data.table
sl3_Task <- R6Class(classname = "sl3_Task",
                    portable = TRUE,
                    class = TRUE,
                    public = list(
                      outcome_type = NULL,       # Values of the binary outcome (Ynode) in observed data where det.Y = TRUE obs are set to NA
                      initialize = function(data, covariates, outcome, outcome_type=NULL, id=NULL, weights=NULL, folds=NULL, nodes=NULL, column_names=NULL, row_index=NULL) {
                        assert_that(is.data.frame(data) | is.data.table(data))
                        private$.data <- data
                        
                        
                        if(!inherits(data,"data.table")){
                          setDT(private$.data)
                        }
                        
                        if(is.null(nodes)){
                          nodes <- list(covariates=covariates,
                                        outcome = outcome,
                                        id = id,
                                        weights= weights)
                        } else {
                          # todo: validate node schema
                        }
                        
                        if(is.null(column_names)){
                          column_names <- as.list(names(data))
                          names(column_names) <- column_names
                        }
                        private$.row_index <- row_index
                        private$.column_names <- column_names
                        
                        #verify nodes are contained in dataset
                        all_nodes <- unlist(nodes[c("covariates","outcome","id","weights")])
                        missing_cols <- setdiff(all_nodes,names(column_names))
  
                        assert_that(length(missing_cols)==0, msg = sprintf("Couldn't find %s",paste(missing_cols,collapse=" ")))
                        
                        private$.nodes <- nodes
                        
                        if(is.null(folds)){
                          folds <- make_folds(cluster_ids=self$id)
                          
                        }
                        
                        private$.folds <- folds
                        
                        
                        private$.uuid <- UUIDgenerate(use.time=T)
                        
                        invisible(self)
                      },
                      
                      add_interactions = function(interactions) {
                        ## -----------------------------------------------------------------------------
                        ## Add columns with interactions (by reference) to input design matrix (data.table). Used for training / predicting.
                        ## returns the names of the added columns
                        ## -----------------------------------------------------------------------------
                        data.table::setDF(private$.data)
                        data.table::setDT(private$.data)
                        
                        prod.DT <- function(x) {
                          y <- x[[1]]
                          for(i in 2:ncol(x))
                            y <- y*x[[i]]
                          return(y)
                        }
                        
                        old_names <- names(private$.data)
                        interaction_names <- names(interactions)
                        for (i in seq_along(interactions)) {
                          interact <- interactions[[i]]
                          name <- interaction_names[i]
                          
                          if (is.null(name) || is.na(name)){
                            name <-  interaction_names[i] <- paste0(interact, collapse = "_")
                          } 
                          
                          if(name %in% old_names){
                            # this column is already defined, so warn but don't recalculate
                            warning(sprintf("Interaction column %s is already defined, so skipping",name))
                          } else if (all(interact %in% old_names)){
                            private$.data[, (name) := prod.DT(.SD), .SD = interact]
                          }
                        }
                        
                        
                        interaction_names <- intersect(interaction_names, names(private$.data)) #drop interactions that didn't get made
                        private$.column_names[interaction_names] <- interaction_names
                        new_covariates <- c(self$nodes$covariates, interaction_names)
                        return(self$next_in_chain(covariates = new_covariates))
                      },
                      
                      add_columns=function(fit_uuid, new_data, global_cols = FALSE){
                        data = private$.data
                        current_cols = names(data)
                        
                        if(!(is.data.frame(new_data) | is.data.table(new_data))){
                          new_data=as.data.table(new_data)
                        }
                        
                        setDT(new_data)
                        
                        col_names = names(new_data)
                        original_names = copy(col_names)
                        
                        if(!global_cols){
                          #by default prepend column names with fit_uuid to prevent column name conflicts for multiple fits from same learner
                          col_names = paste(fit_uuid, original_names, sep="_")
                          # setnames(new_data, original_names, col_names)
                          
                        }
                        
                        column_names = private$.column_names
                        column_names[original_names] = col_names

                        if(is.null(private$.row_index)){
                          set(data, j=col_names, value=new_data)
                        } else{
                          set(data, i=private$.row_index, j=col_names, value=new_data)
                        }
                        
                        # return an updated column_names map
                        return(column_names)
                        
                      },
                      
                      next_in_chain=function(covariates=NULL, outcome=NULL, id=NULL, weights=NULL, column_names=NULL, new_nodes=NULL){
                        if(is.null(new_nodes)){
                          new_nodes=self$nodes
                          
                          if(!is.null(covariates)){
                            new_nodes$covariates=covariates
                          }
                          
                          if(!is.null(outcome)){
                            new_nodes$outcome=outcome
                          }
                          
                          if(!is.null(id)){
                            new_nodes$id=id
                          }
                          
                          if(!is.null(weights)){
                            new_nodes$weights=weights
                          }
                        }
                        
                        if(is.null(column_names)){
                          column_names <- private$.column_names
                        }
                        all_nodes=unlist(new_nodes[c("covariates","outcome","id","weights")])
                        
                        #verify nodes are contained in dataset
                        missing_cols <- setdiff(all_nodes,names(column_names))

                        assert_that(length(missing_cols)==0, msg = sprintf("Couldn't find %s",paste(missing_cols,collapse=" ")))
                        
                        new_task=self$clone()
                        new_task$initialize(private$.data,nodes=new_nodes, folds = self$folds, column_names = column_names, row_index = private$.row_index)
                        
                        return(new_task)
                      },
                      subset_data = function(rows = NULL, columns){
                        if(missing(rows)){
                          rows = private$.row_index
                        }
                        if(!is.null(rows)){
                          return(private$.data[rows, columns, with=FALSE])
                        } else {
                          return(private$.data[, columns, with=FALSE])
                        }
                      },
                      get_node = function(node_name, generator_fun = NULL){
                        if(missing(generator_fun)){
                          generator_fun <- function(node_name, n){
                            stop(sprintf("Node %s not specified", node_name))
                          }
                        }
                        node_var <- private$.nodes[[node_name]]
                        if(is.null(node_var)){
                          return(generator_fun(node_name, self$nrow))
                        } else{
                          data_col <- self$subset_data(,node_var)
                          return(unlist(data_col, use.names = FALSE))
                        }
                      }),
                    
                    active = list(
                      data = function() {
                        return(private$.data)
                      },
                      nrow = function(){
                        if(is.null(private$.row_index)){
                          return(nrow(private$.data))
                        } else{
                          return(length(private$.row_index))
                        }
                      },
                      nodes = function(){
                        return(private$.nodes)
                      },
                      X = function(){
                        
                        covariates =  private$.nodes$covariates
                        covariate_cols <- unlist(private$.column_names[covariates])
                        X_dt = self$subset_data(, covariate_cols)
                        if(ncol(X_dt)>0){
                          data.table::setnames(X_dt, covariate_cols, covariates)
                        }
                        
                        return(X_dt)
                      },
                      X_intercept = function(){
                        # returns X matrix with manually generated intercept column
                        X_dt=self$X

                        if(ncol(X_dt)==0){
                          intercept=rep(1,self$nrow)
                          X_dt=self$data[,list(intercept=intercept)]
                        } else {
                          X_dt[,intercept:=1]
                        }
                        
                        return(X_dt)
                      },
                      Y = function(){
                        return(self$get_node("outcome"))
                      },
                      weights = function(){
                        return(self$get_node("weights",function(node_var,n){rep(1,n)}))
                      },
                      id = function(){
                        return(self$get_node("id",function(node_var,n){seq_len(n)}))
                      },
                      folds = function(){
                        return(private$.folds)
                      },
                      uuid = function(){
                        return(private$.uuid)
                      },
                      column_names = function(){
                        return(private$.column_names)
                      }),
                    private = list(
                      .data = NULL,
                      .nodes = NULL,
                      .X = NULL,
                      .folds = NULL,
                      .uuid = NULL,
                      .column_names = NULL,
                      .row_index = NULL
                    )
)

#' @export
`[.sl3_Task` <- function(x,i=NULL,j=NULL,...) {
  sl3_Task$new(x$data,nodes=x$nodes, folds=NA,row_index = i)
}