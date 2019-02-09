#' Estimate Cross-Validated Risk of Super Learner
#' @param lrnr_sl a Lrnr_sl object specifying the Super Learner
#' @param task the task used for training and performance assessment
#' @param loss_fun the loss function used to evaluate Super Learner
#' @export
CV_lrnr_sl <- function(lrnr_sl, task, loss_fun) {
  if (!inherits(lrnr_sl, "Lrnr_sl")) {
    stop("lrnr_sl must be a Lrnr_sl object")
  }



  cv_sl <- make_learner(Lrnr_cv, lrnr_sl, full_fit = TRUE)


  cv_sl_fit <- cv_sl$train(task)
  #
  # # to avoid refitting the stack to the full data,
  # # extract the full data stack fits and combine
  # fold_fits <- cv_sl_fit$fit_object$fold_fits
  #
  # combined_fold_fits <- lapply(fold_fits, function(fold_fit){
  #   full_fit <- fold_fit$fit_object$full_fit
  #   full_stack <- fold_fit$fit_object$cv_fit$fit_object$full_fit
  #   stack_combined <- make_learner(Stack, full_stack, full_fit)
  # })
  # combined_fit_object <- cv_sl_fit$fit_object
  # combined_fit_object$fold_fits <- combined_fold_fits
  # cv_combined_fit <- copy(cv_sl)
  # cv_combined_fit$set_train(combined_fit_object, task)
  #

  full_fit <- cv_sl_fit$fit_object$full_fit

  # TODO: extract loss function from lrnr_sl where possible
  full_risk <- full_fit$cv_risk(loss_fun)
  sl_risk <- cv_sl_fit$cv_risk(loss_fun)

  # replace revere cv sl risk with nested cv sl risk
  stack_risks <- full_risk[full_risk$learner != "SuperLearner"]
  set(sl_risk, , "learner", "SuperLearner")
  risks <- rbind(stack_risks, sl_risk)

  return(risks)
}
