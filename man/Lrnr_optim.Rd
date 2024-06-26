% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Lrnr_optim.R
\docType{class}
\name{Lrnr_optim}
\alias{Lrnr_optim}
\title{Optimize Metalearner according to Loss Function using optim}
\format{
\code{\link{R6Class}} object.
}
\value{
Learner object with methods for training and prediction. See
\code{\link{Lrnr_base}} for documentation on learners.
}
\description{
This meta-learner provides fitting procedures for any pairing of loss
function and metalearner function, subject to constraints. The
optimization problem is solved by making use of \code{optim}, For further
details, consult the documentation of \code{\link{optim}}.
}
\section{Parameters}{

\describe{
\item{\code{learner_function=metalearner_linear}}{A function(alpha, X) that
takes a vector of covariates and a matrix of data and combines them into
a vector of predictions. See \link{metalearners} for options.}
\item{\code{loss_function=loss_squared_error}}{A function(pred, truth) that
takes prediction and truth vectors and returns a loss vector. See
\link{loss_functions} for options.}
\item{\code{intercept=FALSE}}{If true, X includes an intercept term.}
\item{\code{init_0=FALSE}}{If true, alpha is initialized to all 0's, useful
for TMLE. Otherwise, it is initialized to equal weights summing to 1,
useful for Super Learner.}
\item{\code{...}}{Not currently used.}
}
}

\section{Common Parameters}{


Individual learners have their own sets of parameters. Below is a list of shared parameters, implemented by \code{Lrnr_base}, and shared
by all learners.

\describe{
\item{\code{covariates}}{A character vector of covariates. The learner will use this to subset the covariates for any specified task}
\item{\code{outcome_type}}{A \code{\link{variable_type}} object used to control the outcome_type used by the learner. Overrides the task outcome_type if specified}
\item{\code{...}}{All other parameters should be handled by the invidual learner classes. See the documentation for the learner class you're instantiating}
}
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
\code{\link{Lrnr_glm}},
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
