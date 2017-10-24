

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
                      initialize = function(data, covariates, outcome = NULL, outcome_type = NULL, outcome_levels = NULL,
                                            id = NULL, weights = NULL, offset = NULL, nodes = NULL, column_names = NULL,
                                            row_index = NULL, folds = NULL) {

                        # process data
                        assert_that(is.data.frame(data) | is.data.table(data))
                        private$.data <- data

                        if(!inherits(data,"data.table")){
                          setDT(private$.data)
                        }

                        # process column_names
                        if(is.null(column_names)){
                          column_names <- as.list(names(data))
                          names(column_names) <- column_names
                        }

                        private$.column_names <- column_names

                        # generate node list from other arguments
                        if(is.null(nodes)){
                          nodes <- list(covariates=covariates,
                                        outcome = outcome,
                                        id = id,
                                        weights = weights,
                                        offset = offset)
                        } else {
                          # todo: validate node schema
                        }

                        # verify nodes are contained in dataset
                        all_nodes <- unlist(nodes[c("covariates","outcome","id","weights")])
                        missing_cols <- setdiff(all_nodes,names(column_names))

                        assert_that(length(missing_cols)==0, msg = sprintf("Couldn't find %s",paste(missing_cols,collapse=" ")))

                        private$.nodes <- nodes


                        # process outcome type
                        if(is.character(outcome_type)){
                          outcome_type <- variable_type(type = outcome_type, levels = outcome_levels, x = self$Y)
                        } else if(is.null(outcome_type)){
                          if(!is.null(nodes$outcome)){
                            outcome_type <- variable_type(x = self$Y)
                          } else {
                            outcome_type <- variable_type("none")
                          }
                        }

                        private$.outcome_type <- outcome_type

                        # process row_index
                        private$.row_index <- row_index

                        private$.folds <- folds

                        # assign uuid
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

                      next_in_chain=function(covariates=NULL, outcome=NULL, id=NULL, weights=NULL, 
                                             offset=NULL, column_names=NULL, new_nodes=NULL, ...){
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
                          
                          if(!is.null(offset)){
                            new_nodes$offset=offset
                          }
                          
                        }

                        if(is.null(column_names)){
                          column_names <- private$.column_names
                        }
                        all_nodes=unlist(new_nodes[c("covariates","outcome","id","weights", "offset")])

                        #verify nodes are contained in dataset
                        missing_cols <- setdiff(all_nodes,names(column_names))

                        assert_that(length(missing_cols)==0, msg = sprintf("Couldn't find %s",paste(missing_cols,collapse=" ")))

                        new_task=self$clone()


                        if((is.null(new_nodes$outcome)&&is.null(self$nodes$outcome))||
                           (new_nodes$outcome==self$nodes$outcome)){
                          # if we have the same outcome, transfer outcome properties
                          new_outcome_type <- self$outcome_type
                        } else {
                          # otherwise, let the new task guess
                          new_outcome_type <- NULL
                        }
                        new_task$initialize(private$.data,nodes=new_nodes, folds = private$.folds,
                                            column_names = column_names, row_index = private$.row_index,
                                            outcome_type = new_outcome_type, ...)

                        return(new_task)
                      },
                      subset_task = function(row_index){
                        old_row_index <- private$.row_index
                        if(!is.null(old_row_index)){
                          #index into the logical rows of this task
                          row_index <- old_row_index[row_index]
                        }
                        new_task=self$clone()
                        new_task$initialize(private$.data,nodes=private$.nodes, folds = self$folds,
                                            column_names = private$.column_names, row_index = row_index,
                                            outcome_type = self$outcome_type)

                        return(new_task)
                      },
                      get_data = function(rows = NULL, columns){



                        if(missing(rows)){
                          rows = private$.row_index
                        }

                        true_columns <- unlist(private$.column_names[columns])

                        if(!is.null(rows)){
                          subset <- private$.data[rows, true_columns, with=FALSE]
                        } else {
                          subset <- private$.data[, true_columns, with=FALSE]
                        }

                        if(ncol(subset)>0){
                          data.table::setnames(subset, true_columns, columns)
                        }

                        return(subset)
                      },
                      has_node = function(node_name){
                        node_var <- private$.nodes[[node_name]]
                        return(!is.null(node_var))
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
                          data_col <- self$get_data(,node_var)

                          if(ncol(data_col)==1){
                            return(unlist(data_col, use.names = FALSE))
                          } else {
                            return(data_col)
                          }
                        }
                      },
                      print = function(){
                        cat(sprintf("A sl3 Task with %d observations and the following nodes:\n", self$nrow))
                        print(self$nodes)
                      }
                    ),
                    active = list(
                      raw_data = function(){
                        return(private$.data)
                      },
                      data = function() {
                        all_nodes <- unlist(private$.nodes)

                        return(self$get_data(,all_nodes))
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
                        X_dt = self$get_data(, covariates)

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
                      offset = function(){
                        return(self$get_node("offset"))
                      },
                      weights = function(){
                        return(self$get_node("weights",function(node_var,n){rep(1,n)}))
                      },
                      id = function(){
                        return(self$get_node("id",function(node_var,n){seq_len(n)}))
                      },
                      folds = function(new_folds){

                        if(!missing(new_folds)){
                          private$.folds <- new_folds
                        } else if(is.null(private$.folds)){
                          # generate folds now if never specified
                          if(self$has_node("id")){
                            new_folds <- make_folds(cluster_ids=self$id)
                          } else {
                            new_folds <- make_folds(n=self$nrow)
                          }

                          private$.folds <- new_folds

                        }

                        return(private$.folds)
                      },
                      uuid = function(){
                        return(private$.uuid)
                      },
                      column_names = function(){
                        return(private$.column_names)
                      },
                      outcome_type = function(){
                        return(private$.outcome_type)
                      }
                    ),
                    private = list(
                      .data = NULL,
                      .nodes = NULL,
                      .X = NULL,
                      .folds = NULL,
                      .uuid = NULL,
                      .column_names = NULL,
                      .row_index = NULL,
                      .outcome_type = NULL
                    )
)

#' @export
`[.sl3_Task` <- function(x,i=NULL,j=NULL,...) {
  return(x$subset_task(i))
}