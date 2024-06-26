% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Lrnr_glm.R
\docType{class}
\name{Lrnr_glm}
\alias{Lrnr_glm}
\title{Generalized Linear Models}
\format{
An \code{\link[R6]{R6Class}} object inheriting from
\code{\link{Lrnr_base}}.
}
\value{
A learner object inheriting from \code{\link{Lrnr_base}} with
methods for training and prediction. For a full list of learner
functionality, see the complete documentation of \code{\link{Lrnr_base}}.
}
\description{
This learner provides fitting procedures for generalized linear models using
the \pkg{stats} package \code{\link[stats]{glm.fit}} function.
}
\section{Parameters}{

\itemize{
\item \code{intercept = TRUE}: Should an intercept be included in the model?
\item \code{...}: Other parameters passed to \code{\link[stats]{glm}} or
\code{\link[stats]{glm.fit}}.
}
}

\examples{
data(cpp_imputed)
covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")

# simple, main-terms GLM
lrnr_glm <- make_learner(Lrnr_glm)
glm_fit <- lrnr_glm$train(task)
glm_preds <- glm_fit$predict()

# We can include interaction terms by 'piping' them into this learner.
# Note that both main terms and the specified interactions will be included
# in the regression model.
interaction <- list(c("apgar1", "parity"))
lrnr_interaction <- Lrnr_define_interactions$new(interactions = interaction)
lrnr_glm_w_interaction <- make_learner(Pipeline, lrnr_interaction, lrnr_glm)
fit <- lrnr_glm_w_interaction$train(task)
coefs <- coef(fit$learner_fits$Lrnr_glm_TRUE)
}
\seealso{
Other Learners: 
\code{\link{Custom_chain}},
\code{\link{Lrnr_HarmonicReg}},
\code{\link{Lrnr_arima}},
\code{\link{Lrnr_bartMachine}},
\code{\link{Lrnr_base}},
\code{\link{Lrnr_bayesglm}},
\code{\link{Lrnr_caret}},
\code{\link{Lrnr_cv_selector}},
\code{\link{Lrnr_cv}},
\code{\link{Lrnr_dbarts}},
\code{\link{Lrnr_define_interactions}},
\code{\link{Lrnr_density_discretize}},
\code{\link{Lrnr_density_hse}},
\code{\link{Lrnr_density_semiparametric}},
\code{\link{Lrnr_earth}},
\code{\link{Lrnr_expSmooth}},
\code{\link{Lrnr_gam}},
\code{\link{Lrnr_ga}},
\code{\link{Lrnr_gbm}},
\code{\link{Lrnr_glm_fast}},
\code{\link{Lrnr_glm_semiparametric}},
\code{\link{Lrnr_glmnet}},
\code{\link{Lrnr_glmtree}},
\code{\link{Lrnr_grfcate}},
\code{\link{Lrnr_grf}},
\code{\link{Lrnr_gru_keras}},
\code{\link{Lrnr_gts}},
\code{\link{Lrnr_h2o_grid}},
\code{\link{Lrnr_hal9001}},
\code{\link{Lrnr_haldensify}},
\code{\link{Lrnr_hts}},
\code{\link{Lrnr_independent_binomial}},
\code{\link{Lrnr_lightgbm}},
\code{\link{Lrnr_lstm_keras}},
\code{\link{Lrnr_mean}},
\code{\link{Lrnr_multiple_ts}},
\code{\link{Lrnr_multivariate}},
\code{\link{Lrnr_nnet}},
\code{\link{Lrnr_nnls}},
\code{\link{Lrnr_optim}},
\code{\link{Lrnr_pca}},
\code{\link{Lrnr_pkg_SuperLearner}},
\code{\link{Lrnr_polspline}},
\code{\link{Lrnr_pooled_hazards}},
\code{\link{Lrnr_randomForest}},
\code{\link{Lrnr_ranger}},
\code{\link{Lrnr_revere_task}},
\code{\link{Lrnr_rpart}},
\code{\link{Lrnr_rugarch}},
\code{\link{Lrnr_screener_augment}},
\code{\link{Lrnr_screener_coefs}},
\code{\link{Lrnr_screener_correlation}},
\code{\link{Lrnr_screener_importance}},
\code{\link{Lrnr_sl}},
\code{\link{Lrnr_solnp_density}},
\code{\link{Lrnr_solnp}},
\code{\link{Lrnr_stratified}},
\code{\link{Lrnr_subset_covariates}},
\code{\link{Lrnr_svm}},
\code{\link{Lrnr_tsDyn}},
\code{\link{Lrnr_ts_weights}},
\code{\link{Lrnr_xgboost}},
\code{\link{Pipeline}},
\code{\link{Stack}},
\code{\link{define_h2o_X}()},
\code{\link{undocumented_learner}}
}
\concept{Learners}
\keyword{data}
