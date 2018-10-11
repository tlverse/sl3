#' Plot predicted and true values for diganostic purposes
#'
#' If a Lrnr_sl fit is provided, predictions will be generated from the cross-validated learner fits and final metalearner fit.
#' Otherwise, non cross-validated predictions will be used an an error will be thrown
#'
#' @param learner_fit A fit sl3 learner object. Ideally from a Lrnr_sl
#'
#' @return A ggplot2 object
#'
#' @export
#' @import ggplot2
prediction_plot <- function(learner_fit) {
  # kludge for Rcmd::check with data.table:
  # see https://github.com/Rdatatable/data.table/issues/850
  accurate <- category <- value <- NULL
  training_task <- learner_fit$training_task
  outcome_type <- training_task$outcome_type

  if (!is.null(learner_fit$fit_object$cv_meta_fit)) {
    predictions <- learner_fit$fit_object$cv_meta_fit$predict()
  } else {
    warning("Not a Lrnr_sl, using resubstiution predictions.")
    predictions <- learner_fit$predict()
  }

  observed <- training_task$Y

  if (outcome_type$type == "continuous") {
    pred_data <- data.table(pred = predictions, obs = observed)
    pred_plot <- ggplot(pred_data, aes_(x = ~obs, y = ~pred)) + geom_point() + geom_abline() +
      xlab("Observed") + ylab("Predicted") + theme_bw() + geom_smooth(se = FALSE)
  } else {
    unpacked <- unpack_predictions(predictions)
    unpacked <- as.data.table(unpacked)
    setnames(unpacked, outcome_type$levels)
    unpacked[, observed := observed]

    long <- melt.data.table(unpacked, id.vars = c("observed"), measure.vars = outcome_type$levels, variable.name = "category")
    cutoffs <- seq(from = 0, to = 1, length = 1000)
    long[, accurate := category == observed]
    all_auc_data <- lapply(cutoffs, function(cutoff) {
      auc_data <- long[, list(positive_rate = mean(value > cutoff), cutoff = cutoff), by = list(observed, accurate)]
    })
    auc_data <- rbindlist(all_auc_data)
    wide <- dcast(auc_data, cutoff + observed ~ accurate, value.var = "positive_rate")
    pred_plot <- ggplot(wide[order(wide$`TRUE`)], aes_(x = ~`FALSE`, y = ~`TRUE`, color = ~observed)) + geom_step(direction = "vh") +
      geom_segment(data = data.table(1), x = 0, y = 0, xend = 1, yend = 1, color = "black") +
      theme_bw() + theme(legend.position = "bottom") +
      xlab("False Positive Rate") + ylab("True Positive Rate") + scale_color_discrete("Observed") + coord_equal()
  }

  return(pred_plot)
}
