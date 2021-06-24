#' Define a Machine Learning Task
#'
#' An increasingly less thin wrapper around a \code{data.table} containing the
#' data. Contains metadata about the particular machine learning problem,
#' including which variables are to be used as covariates and outcomes.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.count is.flag
#' @importFrom origami make_folds
#' @importFrom uuid UUIDgenerate
#' @importFrom digest digest
#' @import data.table
#'
#' @export
#'
#' @keywords data
#'
#' @return \code{sl3_Task} object
#'
#' @format \code{\link{R6Class}} object.
#'
#' @template sl3_Task_extra
#
sl3_Task <- R6Class(
  classname = "sl3_Task",
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(data, covariates, outcome = NULL,
                          outcome_type = NULL, outcome_levels = NULL,
                          id = NULL, weights = NULL, offset = NULL, time = NULL,
                          nodes = NULL, column_names = NULL, row_index = NULL,
                          folds = NULL, flag = TRUE,
                          drop_missing_outcome = FALSE) {


      # generate node list from other arguments if not explicitly specified
      if (is.null(nodes)) {
        nodes <- list(
          covariates = covariates, outcome = outcome, id = id,
          weights = weights, offset = offset, time = time
        )
      }

      # generate column name mapping if not specified
      all_nodes <- unlist(nodes)


      # get column names from data (and check data class in the process)
      if (inherits(data, "data.frame")) {
        data_names <- copy(names(data))
      } else if (inherits(data, "Shared_Data")) {
        data_names <- copy(data$column_names)
      } else {
        stop(sprintf("Data of class %s not supported", class(data)[[1]]))
      }


      if (is.null(column_names)) {
        column_names <- data_names
        names(column_names) <- column_names
      }

      # verify nodes are contained in column map
      missing_cols <- setdiff(all_nodes, names(column_names))

      assert_that(
        length(missing_cols) == 0,
        msg = sprintf(
          "Couldn't find %s",
          paste(missing_cols, collapse = " ")
        )
      )

      # verify referenced columns are actually in data
      referenced_columns <- column_names[all_nodes]

      missing_cols <- setdiff(referenced_columns, data_names)
      assert_that(
        length(missing_cols) == 0,
        msg = sprintf(
          "Data doesn't contain referenced columns %s",
          paste(missing_cols, collapse = " ")
        )
      )

      # process data
      if (inherits(data, "Shared_Data")) {
        # we already have a Shared_Data object, so just store it
        # we don't do processing because we assume it's already been done
        private$.shared_data <- data
      } else {
        # we have some other data object, so construct a Shared_Data object
        # and store it (this will copy the data)

        # process characters and missings
        processed <- process_data(data,
          nodes,
          column_names = column_names,
          flag = flag, drop_missing_outcome = drop_missing_outcome
        )

        data <- processed$data
        nodes <- processed$nodes
        column_names <- processed$column_names

        # process_data copies, so don't copy again here
        private$.shared_data <- Shared_Data$new(data, force_copy = FALSE)
      }

      # store final nodes and column names
      private$.nodes <- nodes
      private$.column_names <- column_names

      # process outcome type
      if (is.character(outcome_type)) {
        outcome_type <- variable_type(
          type = outcome_type,
          levels = outcome_levels, x = self$Y
        )
      } else if (is.null(outcome_type)) {
        if (!is.null(nodes$outcome)) {
          outcome_type <- variable_type(x = self$Y)
        } else {
          outcome_type <- variable_type("none")
        }
      }
      private$.outcome_type <- outcome_type

      # process row_index
      private$.row_index <- row_index
      private$.folds <- folds

      # assign uuid using digest
      private$.uuid <- digest(self$data)

      invisible(self)
    },
    add_interactions = function(interactions, warn_on_existing = TRUE) {
      ## ----------------------------------------------------------------------
      ## Add columns with interactions (by reference) to input design matrix
      ## (data.table). Used for training / predicting.
      ## returns the names of the added columns
      ## ----------------------------------------------------------------------
      prod.DT <- function(x) {
        y <- x[[1]]
        for (i in 2:ncol(x)) {
          y <- y * x[[i]]
        }
        return(y)
      }

      old_names <- self$column_names
      interaction_names <- names(interactions)
      if (is.null(interaction_names)) {
        interaction_names <- sapply(interactions, paste0, collapse = "_")
      }
      is_new <- !(interaction_names %in% old_names)
      if (any(!is_new)) {
        warning(
          "The following interactions already exist:",
          paste0(interaction_names[!is_new], collapse = ", ")
        )
      }
      interaction_data <- lapply(interactions[is_new], function(int) {
        # check if interaction terms numeric
        int_numeric <- sapply(int, function(i) is.numeric(self$X[[i]]))
        if (all(int_numeric)) {
          d_int <- data.table(self$X[, prod.DT(.SD), .SD = int])
          setnames(d_int, paste0(int, collapse = "_"))
          return(d_int)
        } else {
          # match interaction terms to X
          Xmatch <- lapply(int, function(i) grep(i, colnames(self$X), value = T))
          Xint <- as.list(as.data.frame(t(expand.grid(Xmatch))))

          d_Xint <- lapply(Xint, function(Xint) self$X[, prod.DT(.SD), .SD = Xint])
          setDT(d_Xint)
          setnames(d_Xint, sapply(Xint, paste0, collapse = "_"))

          no_Xint <- rowSums(d_Xint) == 0 # happens when we omit 1 factor level
          if (any(int_numeric)) {
            d_Xint$other <- rep(0, nrow(d_Xint))
            d_Xint[no_Xint, "other"] <- 1
            if (any(int_numeric)) {
              # we actually need to take the product if we have a numeric covariate
              d_Xint[no_Xint, "other"] <- prod.DT(data.table(
                rep(1, sum(no_Xint)),
                self$X[no_Xint, names(which(int_numeric)), with = F]
              ))
            }
            other_name <- paste0("other.", paste0(int, collapse = "_"))
            colnames(d_Xint)[ncol(d_Xint)] <- other_name
          }
          return(d_Xint)
        }
      })

      interaction_names <- unlist(lapply(interaction_data, colnames))
      interaction_data <- data.table(do.call(cbind, interaction_data))
      setnames(interaction_data, interaction_names)

      interaction_cols <- self$add_columns(interaction_data, column_uuid = NULL)
      new_covariates <- c(self$nodes$covariates, interaction_names)
      return(self$next_in_chain(
        covariates = new_covariates,
        column_names = interaction_cols
      ))
    },
    add_columns = function(new_data, column_uuid = uuid::UUIDgenerate()) {
      if (is.numeric(private$.row_index)) {
        new_col_map <- private$.shared_data$add_columns(
          new_data, column_uuid,
          as.integer(private$.row_index)
        )
      } else {
        new_col_map <- private$.shared_data$add_columns(
          new_data, column_uuid,
          private$.row_index
        )
      }

      column_names <- private$.column_names
      column_names[names(new_col_map)] <- new_col_map

      # return an updated column_names map
      return(column_names)
    },
    next_in_chain = function(covariates = NULL, outcome = NULL, id = NULL,
                             weights = NULL, offset = NULL, time = NULL,
                             folds = NULL, column_names = NULL,
                             new_nodes = NULL, ...) {
      if (is.null(new_nodes)) {
        new_nodes <- self$nodes

        if (!is.null(covariates)) {
          new_nodes$covariates <- covariates
        }

        if (!is.null(outcome)) {
          new_nodes$outcome <- outcome
        }

        if (!missing(id)) {
          new_nodes$id <- id
        }

        if (!missing(weights)) {
          new_nodes$weights <- weights
        }

        if (!missing(offset)) {
          new_nodes$offset <- offset
        }

        if (!missing(time)) {
          new_nodes$time <- time
        }
      }

      if (is.null(column_names)) {
        column_names <- private$.column_names
      }

      if (is.null(folds)) {
        folds <- private$.folds
      }

      all_nodes <- unlist(new_nodes)

      # verify nodes are contained in dataset
      missing_cols <- setdiff(all_nodes, names(column_names))

      assert_that(
        length(missing_cols) == 0,
        msg = sprintf(
          "Couldn't find %s",
          paste(missing_cols, collapse = " ")
        )
      )
      new_task <- self$clone()

      if ((is.null(new_nodes$outcome) &&
        is.null(self$nodes$outcome)) ||
        all(new_nodes$outcome == self$nodes$outcome)) {
        # if we have the same outcome, transfer outcome properties
        new_outcome_type <- self$outcome_type
      } else {
        # otherwise, let the new task guess
        new_outcome_type <- NULL
      }
      new_task$initialize(
        private$.shared_data,
        nodes = new_nodes,
        folds = folds,
        column_names = column_names,
        row_index = private$.row_index,
        outcome_type = new_outcome_type,
        ...
      )
      return(new_task)
    },
    subset_task = function(row_index, drop_folds = FALSE) {
      if (is.logical(row_index)) {
        row_index <- which(row_index)
      }
      old_row_index <- private$.row_index
      if (!is.null(old_row_index)) {
        # index into the logical rows of this task
        row_index <- old_row_index[row_index]
      }

      must_reindex <- any(duplicated(row_index))
      if (must_reindex) {
        new_shared_data <- private$.shared_data$clone()
        new_shared_data$reindex(row_index)
        row_index <- seq_along(row_index)
      } else {
        new_shared_data <- private$.shared_data
      }

      new_task <- self$clone()
      if (drop_folds) {
        new_folds <- NULL
      } else {
        if (must_reindex) {
          stop("subset indicies have copies, this requires dropping folds.")
        }
        new_folds <- subset_folds(private$.folds, row_index)
      }

      new_task$initialize(
        new_shared_data,
        nodes = private$.nodes,
        folds = new_folds,
        column_names = private$.column_names,
        row_index = row_index,
        outcome_type = self$outcome_type
      )
      return(new_task)
    },
    get_data = function(rows = NULL, columns, expand_factors = FALSE) {
      if (missing(rows)) {
        rows <- private$.row_index
      }

      true_columns <- unlist(private$.column_names[columns])

      subset <- private$.shared_data$get_data(rows, true_columns)

      if (ncol(subset) > 0) {
        data.table::setnames(subset, true_columns, columns)
      }

      if (expand_factors) {
        subset <- dt_expand_factors(subset)
      }
      return(subset)
    },
    has_node = function(node_name) {
      node_var <- private$.nodes[[node_name]]
      return(!is.null(node_var))
    },
    get_node = function(node_name, generator_fun = NULL,
                        expand_factors = FALSE) {
      if (missing(generator_fun)) {
        generator_fun <- function(node_name, n) {
          stop(sprintf("Node %s not specified", node_name))
        }
      }
      node_var <- private$.nodes[[node_name]]

      if (is.null(node_var)) {
        return(generator_fun(node_name, self$nrow))
      } else {
        data_col <- self$get_data(, node_var, expand_factors)

        if (ncol(data_col) == 1) {
          return(unlist(data_col, use.names = FALSE))
        } else {
          return(data_col)
        }
      }
    },
    offset_transformed = function(link_fun = NULL, for_prediction = FALSE) {
      if (self$has_node("offset")) {
        offset <- self$offset

        # transform if sl3.transform.offset is true and link_fun was provided
        if (getOption("sl3.transform.offset") && !is.null(link_fun)) {
          offset <- link_fun(offset)
        }
      } else {
        # if task has no offset, return NULL or a zero offset as is appropriate
        stop("Trained with offsets but predict method called on task without.")
      }
      return(offset)
    },
    print = function() {
      cat(sprintf("A sl3 Task with %d obs and these nodes:\n", self$nrow))
      print(self$nodes)
    },
    revere_fold_task = function(fold_number) {
      return(self)
    }
  ),
  active = list(
    internal_data = function() {
      return(private$.shared_data)
    },
    data = function() {
      all_nodes <- unique(unlist(private$.nodes))
      return(self$get_data(, all_nodes))
    },
    nrow = function() {
      if (is.null(private$.row_index)) {
        return(private$.shared_data$nrow)
      } else {
        return(length(private$.row_index))
      }
    },
    nodes = function() {
      return(private$.nodes)
    },
    X = function() {
      covariates <- private$.nodes$covariates
      X_dt <- self$get_data(, covariates, expand_factors = TRUE)
      return(X_dt)
    },
    X_intercept = function() {
      # returns X matrix with manually generated intercept column
      X_dt <- self$X

      if (ncol(X_dt) == 0) {
        intercept <- rep(1, self$nrow)
        X_dt <- self$data[, list(intercept = intercept)]
      } else {
        old_ncol <- ncol(X_dt)
        X_dt[, intercept := 1]

        # make intercept first column
        setcolorder(X_dt, c(old_ncol + 1, seq_len(old_ncol)))
      }

      return(X_dt)
    },
    Y = function() {
      return(self$get_node("outcome"))
    },
    offset = function() {
      return(self$get_node("offset"))
    },
    weights = function() {
      return(self$get_node("weights", function(node_var, n) {
        rep(1, n)
      }))
    },
    id = function() {
      return(self$get_node("id", function(node_var, n) {
        seq_len(n)
      }))
    },
    time = function() {
      return(self$get_node("time", function(node_var, n) {
        if (self$has_node("id")) {
          stop("times requested but not specified for this task")
        } else {
          self$row_index
        }
      }))
    },
    folds = function(new_folds) {
      if (!missing(new_folds)) {
        private$.folds <- new_folds
      } else if (is.numeric(private$.folds) | is.null(private$.folds)) {
        args <- list()
        args$n <- self$nrow
        if (is.numeric(private$.folds)) {
          # pass integer to V argument when it's supplied
          args$V <- private$.folds
        }
        if (self$has_node("id")) {
          # clustered cross-validation for clustered data
          args$cluster_ids <- self$id
        }
        if (self$outcome_type$type %in% c("binomial", "categorical")) {
          # stratified cross-validation folds for discrete outcomes
          args$strata_ids <- self$Y
        }
        if (self$outcome_type$type %in% c("binomial", "categorical") &
          self$has_node("id")) {
          # don't use stratified CV if clusters are not nested in strata
          is_nested <- all(
            rowSums(table(args$cluster_ids, args$strata_ids) > 0) == 1
          )
          if (!is_nested) {
            args <- args[!(names(args) == "strata_ids")]
          }
        }
        new_folds <- do.call(origami::make_folds, args)
        private$.folds <- new_folds
      }
      return(private$.folds)
    },
    uuid = function() {
      return(private$.uuid)
    },
    column_names = function() {
      return(private$.column_names)
    },
    outcome_type = function() {
      return(private$.outcome_type)
    },
    row_index = function() {
      return(private$.row_index)
    }
  ),
  private = list(
    .shared_data = NULL,
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
`[.sl3_Task` <- function(x, i = NULL, j = NULL, ...) {
  return(x$subset_task(i))
}

#' @param ... Passes all arguments to the constructor. See documentation for
#'  Constructor below.
#'
#' @rdname sl3_Task
#'
#' @export
#
make_sl3_Task <- sl3_Task$new
