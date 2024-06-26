% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Lrnr_dbarts.R
\docType{class}
\name{Lrnr_dbarts}
\alias{Lrnr_dbarts}
\title{Discrete Bayesian Additive Regression Tree sampler}
\format{
\code{\link{R6Class}} object.
}
\value{
Learner object with methods for training and prediction. See
\code{\link{Lrnr_base}} for documentation on learners.
}
\description{
This learner implements BART algorithm in C++, using the \code{dbarts} package.
BART is a Bayesian sum-of-trees model in which each tree is constrained
by a prior to be a weak learner.
}
\section{Parameters}{

\describe{
\item{\code{x.test}}{Explanatory variables for test (out of sample) data.
\code{bart} will generate draws of \eqn{f(x)} for each \eqn{x} which is a
row of \code{x.test}.}
\item{\code{sigest}}{For continuous response models, an estimate of the
error variance, \eqn{\sigma^2}, used to calibrate an inverse-chi-squared
prior used on that parameter. If not supplied, the least-squares estimate
is derived instead. See \code{sigquant} for more information. Not
applicable when \eqn{y} is binary.}
\item{\code{sigdf}}{Degrees of freedom for error variance prior. Not
applicable when \eqn{y} is binary.}
\item{\code{sigquant}}{The quantile of the error variance prior that the
rough estimate (\code{sigest}) is placed at. The closer the quantile is
to 1, the more aggresive the fit will be as you are putting more prior
weight on error standard deviations (\eqn{\sigma}) less than the rough
estimate. Not applicable when \eqn{y} is binary.}
\item{\code{k}}{For numeric \eqn{y}, \code{k} is the number of prior
standard deviations \eqn{E(Y|x) = f(x)} is away from \eqn{\pm 0.5}{+/-
    0.5}. The response (\code{y.train}) is internally scaled to range from
\eqn{-0.5} to \eqn{0.5}. For binary \eqn{y}, \code{k} is the number of
prior standard deviations \eqn{f(x)} is away from \eqn{\pm 3}{+/- 3}. In
both cases, the bigger \eqn{k} is, the more conservative the fitting will
be.}
\item{\code{power}}{Power parameter for tree prior.}
\item{\code{base}}{Base parameter for tree prior.}
\item{\code{binaryOffset}}{ sed for binary \eqn{y}. When present, the model
is \eqn{P(Y = 1 \mid x) = \Phi(f(x) + \mathrm{binaryOffset})}{P(Y = 1 |
    x) = \Phi(f(x) + binaryOffset)}, allowing fits with probabilities shrunk
towards values other than \eqn{0.5}.}
\item{\code{weights}}{An optional vector of weights to be used in the
fitting process. When present, BART fits a model with observations
\eqn{y \mid x \sim N(f(x), \sigma^2 / w)}{y | x ~ N(f(x), \sigma^2 / w)},
where \eqn{f(x)} is the unknown function.}
\item{\code{ntree}}{The number of trees in the sum-of-trees formulation.}
\item{\code{ndpost}}{The number of posterior draws after burn in,
\code{ndpost / keepevery} will actually be returned.}
\item{\code{nskip}}{Number of MCMC iterations to be treated as burn in.}
\item{\code{printevery}}{As the MCMC runs, a message is printed every
\code{printevery} draws.}
\item{\code{keepevery}}{Every \code{keepevery} draw is kept to be returned
to the user. Useful for \dQuote{thinning} samples.}
\item{\code{keeptrainfits}}{If \code{TRUE} the draws of \eqn{f(x)} for
\eqn{x} corresponding to the rows of \code{x.train} are returned.}
\item{\code{usequants}}{When \code{TRUE}, determine tree decision rules
using estimated quantiles derived from the \code{x.train} variables. When
\code{FALSE}, splits are determined using values equally spaced across
the range of a variable. See details for more information.}
\item{\code{numcut}}{The maximum number of possible values used in decision
rules (see \code{usequants}, details). If a single number, it is recycled
for all variables; otherwise must be a vector of length equal to
\code{ncol(x.train)}. Fewer rules may be used if a covariate lacks enough
unique values.}
\item{\code{printcutoffs}}{The number of cutoff rules to printed to screen
before the MCMC is run. Given a single integer, the same value will be
used for all variables. If 0, nothing is printed.}
\item{\code{verbose}}{Logical; if \code{FALSE} supress printing.}
\item{\code{nchain}}{Integer specifying how many independent tree sets and
fits should be calculated.}
\item{\code{nthread}}{Integer specifying how many threads to use. Depending
on the CPU architecture, using more than the number of chains can degrade
performance for small/medium data sets. As such some calculations may be
executed single threaded regardless.}
\item{\code{combinechains}}{Logical; if \code{TRUE}, samples will be
returned in arrays of dimensions equal to \code{nchain} \eqn{\times}
\code{ndpost} \eqn{\times} number of observations.}
\item{\code{keeptrees}}{Logical; must be \code{TRUE} in order to use
\code{predict} with the result of a \code{bart} fit.}
\item{\code{keepcall}}{Logical; if \code{FALSE}, returned object will have
\code{call} set to \code{call("NULL")}, otherwise the call used to
instantiate BART.}
\item{\code{serializeable}}{Logical; if \code{TRUE}, loads the trees into R memory
so the fit object can be saved and loaded. See the section on "Saving"
in \code{\link[dbarts]{bart} NB: This is not currently working}
}
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

\examples{
set.seed(123)

# load example data
data(cpp_imputed)
covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")

# create sl3 task
task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "haz")
dbart_learner <- make_learner(Lrnr_dbarts, ndpost = 200)

# train dbart learner and make predictions
dbart_fit <- dbart_learner$train(task)
preds <- dbart_fit$predict()
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
