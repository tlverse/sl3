#' Set Columns in data.table Safely
#'
#' Resolves https://github.com/Rdatatable/data.table/issues/1831
#'
#' @export
#
safe_set <- function(x, i = NULL, j, value) {
  n_alloced <- truelength(x)
  n_used <- ncol(x)
  n_new_cols <- ncol(value)
  if (is.null(n_new_cols)){ n_new_cols <- 1}

  if ((n_used + n_new_cols) > n_alloced) {
    # NSE code to modify object in calling frame
    # borrowed from data.table::`[.data.table`
    # https://github.com/Rdatatable/data.table/blob/master/R/data.table.R#L1167-L1183

    name = substitute(x)

    alloc.col(x)   # always assigns to calling scope; i.e. this scope
    if (is.name(name)) {
      assign(as.character(name), x, parent.frame(), inherits = TRUE)
    } else if (is.call(name) && (name[[1L]] == "$" || name[[1L]] == "[[") && is.name(name[[2L]])) {
      k = eval(name[[2L]], parent.frame(), parent.frame())
      if (is.list(k)) {
        origj = j = if (name[[1L]] == "$") as.character(name[[3L]]) else eval(name[[3L]], parent.frame(), parent.frame())
        if (is.character(j)) {
          if (length(j) != 1L) stop("L[[i]][,:=] syntax only valid when i is length 1, but it's length %d",length(j))
          j = match(j, names(k))
          if (is.na(j)) stop("Item '",origj,"' not found in names of list")
        }
        .Call(data.table:::Csetlistelt, k, as.integer(j), x)
      } else if (is.environment(k) && exists(as.character(name[[3L]]), k)) {
        assign(as.character(name[[3L]]), x, k, inherits = FALSE)
      }
    }
  }
  return(set(x, i, j, value))
}

