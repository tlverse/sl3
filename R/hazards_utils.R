#' @importFrom data.table set setnames
#' @importFrom origami id_folds_to_folds
#'
#' @export
#
conditional_hazards_task <- function(task, is_training, outcome_name, Delta, T_tilde) {
  # repeat task across levels of the outcome
  underlying_data <- data.table::copy(task$internal_data$raw_data)
  row_index <- task$row_index
  if (!is.null(row_index)) {
    underlying_data <- underlying_data[row_index]
  }

  # force ids to exist so that we can use them after repeating the task
  id_name <- paste0(UUIDgenerate(), "_id")
  data.table::set(underlying_data, j = id_name, value = task$id)
  column_names <- c(task$column_names, list(id = id_name))

  # generate repeated task
  # TODO: check
  t_max <- max(T_tilde)
  index <- rep(seq_len(task$nrow), each = t_max)
  repeated_data <- underlying_data[index, ]
  # TODO: split train validation test on new indices; check
  new_folds <- origami::id_folds_to_folds(task$folds, index)

  repeated_task <- task$next_in_chain(
    column_names = column_names,
    data = repeated_data, id = "id",
    folds = new_folds
  )
  # TODO: check
  t = rep(seq_len(t_max), task$nrow)
  repeated_delta <- rep(Delta, each = t_max)
  repeated_t_tilde <- rep(T_tilde, each = t_max)

  # print(length(repeated_delta))
  # print(repeated_delta[1:10])
  # print(length(repeated_task$Y))
  # print(repeated_task$Y[1:10])
  
  N_prev <- ifelse(repeated_t_tilde <= t - 1 & repeated_delta == 1, 1, 0)
  N_t <- ifelse(repeated_t_tilde <= t & repeated_delta == 1, 1, 0)
  dN <- ifelse(N_t == 1 & N_prev == 0, 1, 0)

  A_c_prev <- ifelse(repeated_t_tilde <= t - 1 & repeated_delta == 0, 1, 0)
  A_c_t <- ifelse(repeated_t_tilde <= t & repeated_delta == 0, 1, 0)
  dA_c <- ifelse(A_c_t == 1 & A_c_prev == 0, 1, 0)

  new_columns <- repeated_task$add_columns(data.table(
    t = t,
    dN = dN,
    dA_c = dA_c
  ))
  new_covariates <- c(task$nodes$covariates, "t")
  hazard_task <- repeated_task$next_in_chain(
    column_names = new_columns,
    outcome = outcome_name,
    covariates = new_covariates
  )
  if (!is_training) {
    return(hazard_task)
  }

  subset_index <- which(N_prev == 0 & A_c_prev == 0)
  trimmed_hazard_task <- hazard_task[subset_index, ]
  return(trimmed_hazard_task)
}

# # TODO: speed
# make_long_data <- function(short_data, short_npsem) {
#   n <- dim(short_data)[1]
#   rs <- NULL
#   t_tilde_name <- short_npsem$T_tilde$variables
#   t_max <- max(short_data[, ..t_tilde_name])

#   # change n
#   for (i in 1:n) {
#     # TODO: check
#     t_tilde <- short_data[i, ..t_tilde_name][[1]]
#     delta_name <- short_npsem$Delta$variables
#     delta <- short_data[i, ..delta_name]
#     w_name <- short_npsem$W$variables
#     w <- short_data[i, ..w_name]
#     a_name <- short_npsem$A$variables
#     a <- short_data[i, ..a_name]
#     current_df <- data.frame("t" = 1:t_max)

#     current_df[, w_name] <- rep(w, t_max)
#     current_df[, a_name] <- rep(a, t_max)

#     current_df[, "N"] <- ifelse(t_tilde <= 1:t_max - 1 & rep(delta == 1, t_max), 1, 0)
#     current_df[, "A_c"] <- ifelse(t_tilde <= 1:t_max - 1 & rep(delta == 0, t_max), 1, 0)

#     N_prev <- current_df[, "N"]
#     N_t <- ifelse(t_tilde <= 1:t_max & rep(delta == 1, t_max), 1, 0)
#     current_df[, "dN"] <- ifelse(N_t == 1 & N_prev == 0, 1, 0)

#     A_c_prev <- current_df[, "A_c"]
#     A_c_t <- ifelse(t_tilde <= 1:t_max & rep(delta == 0, t_max), 1, 0)
#     current_df[, "dA_c"] <- ifelse(A_c_t == 1 & A_c_prev == 0, 1, 0)

#     if (i == 1) {
#       rs <- current_df
#     } else {
#       rs <- rbind(rs, current_df)
#     }
#   }
#   return(rs)
# }

# make_long_node_list <- function(short_npsem) {
#   w_name <- short_npsem$W$variables
#   a_name <- short_npsem$A$variables
#   node_list <- list(W = w_name, A = a_name, N = "N", A_c = "A_c", t = "t", dN = "dN", dA_c = "dA_c")
#   return(node_list)
# }

# survival_tx_long_npsem <- function(node_list, variable_types = NULL) {
#   npsem <- list(
#     define_node("W", node_list$W, variable_type = variable_types$W),
#     define_node("A", node_list$A, c("W"), variable_type = variable_types$A),
#     define_node("N", node_list$N, variable_type = variable_types$N),
#     define_node("A_c", node_list$A_c, variable_type = variable_types$A_c),
#     define_node("t", node_list$t, variable_type = variable_types$t),
#     # TODO: whether keep N, A_c
#     define_node("dN", node_list$dN, c("W", "A", "t"), variable_type = variable_types$dN),
#     define_node("dA_c", node_list$dA_c, c("W", "A", "t"), variable_type = variable_types$dA_c)
#     )

#   return(npsem)
# }

# survival_tx_task <- function(data, node_list, make_npsem, variable_types = NULL) {
#   setDT(data)

#   npsem <- make_npsem(node_list, variable_types)

#   if (!is.null(node_list$id)) {
#     tmle_task <- tmle3_Task$new(data, npsem = npsem, id = node_list$id)
#     } else {
#       tmle_task <- tmle3_Task$new(data, npsem = npsem)
#     }

#     return(tmle_task)
# }

# make_long_tmle_task <- function(long_data, long_node_list) {
#   # TODO: function design
#   long_tmle_task <- survival_tx_task(long_data, long_node_list, survival_tx_long_npsem)

#   return(long_tmle_task)
# }
