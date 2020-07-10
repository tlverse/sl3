ts_get_pred_horizon <- function(training_task, prediction_task, check = TRUE) {
  pred_hmax <- max(unique(prediction_task$time))
  train_hmax <- max(unique(training_task$time))
  horizon <- pred_hmax - train_hmax

  if (check && (!(horizon > 0))) {
    stop("All prediction times must occur after training times for this timeseries learner")
  }

  return(horizon)
}

ts_get_requested_preds <- function(training_task, prediction_task, preds) {
  pred_times <- max(training_task$time) + seq_along(preds)
  pred_index <- match(prediction_task$time, pred_times)
  requested_preds <- preds[pred_index]

  return(requested_preds)
}
