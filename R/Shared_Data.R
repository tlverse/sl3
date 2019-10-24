#' Container Class for data.table Shared Between Tasks
#'
#' Mostly to deal with alloc.col shallow copies, but also nice to have a bit more abstraction.
#' @export
Shared_Data <- R6Class(
  classname = "Shared_Data",
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(data, ...) {
      if (inherits(data, "Shared_Data")) {
        stop("Shared_Data passed another Shared_Data object on construction.
              Instead of doing this, use the existing Shared_Data object.")
      }

      if (inherits(data, "data.table")) {
        # explicitly copy existing data.table
        private$.data <- data.table::copy(data)
      } else {
        # coerce to data.table
        # as.data.table will also copy
        private$.data <- as.data.table(data)
      }
    },
    get_data = function(rows = NULL, columns) {
      if (!is.null(rows)) {
        subset <- private$.data[rows, columns, with = FALSE]
      } else {
        subset <- private$.data[, columns, with = FALSE]
      }

      return(subset)
    },
    add_columns = function(new_data, column_uuid = uuid::UUIDgenerate(), row_index = NULL) {
      current_cols <- names(private$.data)

      if (!(is.data.frame(new_data) | is.data.table(new_data))) {
        new_data <- as.data.table(new_data)
      }

      new_col_names <- names(new_data)
      original_names <- copy(new_col_names)

      if (!is.null(column_uuid)) {
        # by default prepend column names with column_uuid to prevent column name
        # conflicts in Shared_Data
        new_col_names <- paste(column_uuid, original_names, sep = "_")
      }

      column_names <- list()
      column_names[original_names] <- new_col_names

      # manually ensure data.table has enough columns allocated
      # https://github.com/Rdatatable/data.table/issues/1831
      n_alloced <- truelength(private$.data)
      n_used <- ncol(private$.data)
      n_new_cols <- ncol(new_data)
      if (is.null(n_new_cols)) {
        n_new_cols <- 1
      }

      if ((n_used + n_new_cols) > n_alloced) {
        private$.data <- alloc.col(private$.data) # always assigns to calling scope; i.e. this scope
      }

      # actually do assignment
      if (is.null(row_index)) {
        set(private$.data, j = new_col_names, value = new_data)
      } else {
        set(private$.data, i = row_index, j = new_col_names, value = new_data)
      }

      # return an updated column_names map
      return(column_names)
    }
  ),
  active = list(
    raw_data = function() {
      return(private$.data)
    },
    column_names = function() {
      return(names(private$.data))
    },
    nrow = function() {
      return(nrow(private$.data))
    }
  ),
  private = list(
    .data = data.table()
  )
)
