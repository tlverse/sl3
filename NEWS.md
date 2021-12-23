# sl3 1.4.4
* Updates to `Lrnr_nnls` to support binary outcomes, including support for
  convexity of the resultant model fit and warnings on prediction quality.
* Refined, clearer documentation for `Lrnr_define_interactions`
* Tweaks to `Lrnr_bound` to better support more flexible bounding for
  continuous outcomes (automatically setting a maximum of infinity).
* Changes to `Lrnr_cv_selector` to support improved computation of the CV-risk,
  averaging the risk strictly across validation/holdout sets.
* Bug fixes for `Lrnr_earth` (improving formals recognition), `Lrnr_glmnet`
  (allowing offsets), and `Lrnr_caret` (reformatting of arguments).

# sl3 1.4.3
* Additional arguments for 'Keras' learners `Lrnr_lstm_keras` and
  `Lrnr_gru_keras` provide support for callback functions list and 2-layer
  networks. Default `callbacks` list provides early stopping criteria with
  respect to 'Keras' defaults and `patience` of 10 epochs. Also, these two
  'Keras' learners now call `args_to_list` upon initialization, and set
  verbose argument according to `options("keras.fit_verbose")` or
  `options("sl3.verbose")`.
* Update `Lrnr_xgboost` to support prediction tasks consisting of one
  observation (e.g., leave-one-out cross-validation).
* Update `Lrnr_sl` by adding a new private slot `.cv_risk` to store the risk
  estimates, using this to avoid unnecessary re-computation in the `print`
  method (the `.cv_risk` slot is populated on the first `print` call, and only
  ever re-printed thereafter).
* Update documentation of `default_metalearner` to use native markdown tables.
* Fix `Lrnr_screener_importance`'s pairing of (a) covariates returned by the
  importance function with (b) covariates as they are defined in the task. This
  issue only arose when discrete covariates were automatically one-hot encoded
  upon task initiation (i.e., when `colnames(task$X) != task$nodes$covariates`).
* Reformat `importance_plot` to plot variables in decreasing order of
  importance, so most important variables are placed at the top of the dotchart.
* Enhanced functionality in `sl3` task's `add_interactions` method to support
  interactions that involve factors. This method is most commonly used by
  `Lrnr_define_interactions`, which is intended for use with another learner
  (e.g., `Lrnr_glmnet` or `Lrnr_glm`) in a `Pipeline`.
* Modified `Lrnr_gam` formula (if not specified by user) to not use `mgcv`'s
  default `k=10` degrees of freedom for each smooth `s` term when there are
  less than `k=10` degrees of freedom. This bypasses an `mgcv::gam` error, and
  tends to be relevant only for small n.
* Added `options(java.parameters = "-Xmx2500m")` and warning message when
  `Lrnr_bartMachine` is initialized, if this option has not already been set.
  This option was incorporated since the default RAM of 500MB for a Java
  virtual machine often errors due to memory issues with `Lrnr_bartMachine`.
* Incorporated `stratify_cv` argument in `Lrnr_glmnet`, which stratifies
  internal cross-validation folds such that binary outcome prevalence in
  training and validation folds roughly matches the prevalence in the training
  task.
* Incorporated `min_screen` argument `Lrnr_screener_coefs`, which tries to
  ensure that at least `min_screen` number of covariates are selected. If this
  argument is specified and the `learner` argument in `Lrnr_screener_coefs` is
  a `Lrnr_glmnet`, then `lambda` is increased until `min_screen` number of
  covariates are selected and a warning is produced. If `min_screen` is
  specified and the `learner` argument in `Lrnr_screener_coefs` is not a
  `Lrnr_glmnet` then it will error.
* Updated `Lrnr_hal9001` to work with v0.4.0 of the `hal9001` package.
* Added `formula` parameter and `process_formula` function to the base
  learner, `Lrnr_base`, whose methods carry over to all other learners. When
  a `formula` is supplied as a learner parameter, the `process_formula
  function constructs a design matrix by supplying the `formula` to
  `model.matrix`. This implementation allows `formula` to be supplied to all
  learners, even those without native `formula` support. The `formula` should
  be an object of class "`formula`", or a character string that can be coerced
  to that class.
* Added factory function for performance-based risks for binary outcomes with
  `ROCR` performance measures `custom_ROCR_risk`. Supports cutoff-dependent and
  scalar `ROCR` performance measures. The risk is defined as 1 - performance,
  and is transformed back to the performance measure in `cv_risk` and
  `importance` functions. This change prompted the revision of argument name
  `loss_fun` and `loss_function` to `eval_fun` and `eval_function`,
  respectively, since the evaluation of predictions relative to the observations
  can be either a risk or a loss function. This argument name change impacted
  the following: `Lrnr_solnp`, `Lrnr_optim`, `Lrnr_cv_selector`, `cv_risk`,
  `importance`, and `CV_Lrnr_sl`.
* Added name attribute to all loss functions, where naming was defined in terms
  of the risk implied by each loss function (i.e., the common name for the
  expected loss). The names in `cv_risk` and `importance` tables now swap "risk"
  with this name attribute.
* Incorporated stratified cross-validation when `folds` are not supplied to the
  `sl3_Task` and the outcome is a discrete (i.e., binary or categorical)
  variable.
* Added to the `importance` method the option to evaluate importance over
  `covariate_groups`, by removing/permuting all covariates in the same group
  together.
* Added `Lrnr_ga` as another metalearner.

# sl3 1.4.2
* Updates to variable importance functionality, including calculation of risk
  ratio and risk differences under covariate deletion or permutation.
* Addition of a `importance_plot` to summarize variable importance findings.
* Additions of new methods `reparameterize` and `retrain` to `Lrnr_base`, which
  allows modification of the covariate set while training on a conserved task
  and prediction on a new task using previously trained learners, respectively.

# sl3 1.4.1
[missing]

# sl3 1.4.0
[missing]

# sl3 1.3.9
[missing]

# sl3 1.3.8
* Updates to variable importance functionality, including use of risk ratios.
* Change `Lrnr_hal9001` and `Lrnr_glmnet` to respect observation-level IDs.
* Removal of `Remotes` and deprecation of `Lrnr_rfcde` and `Lrnr_condensier`:
  * Both of these learner classes provided support for conditional density
      estimation (CDE) and were useful when support for CDE was more limited.
      Unfortunately, both packages are un-maintained or updated only very
      sporadically, resulting in both frequent bugs and presenting an obstacle
      for an eventual CRAN release (both packages are GitHub-only).
  * `Lrnr_rfcde` wrapped https://github.com/tpospisi/RFCDE, a sporadically
      maintained tool for conditional density estimation (CDE). Support for
      this has been removed in favor of built-in CDE tools, including, among
      others, `Lrnr_density_semiparametric`.
  * `Lrnr_condensier` wrapped https://github.com/osofr/condensier, which
      provided a pooled hazards approach to CDE. This package contained an
      implementation error (https://github.com/osofr/condensier/issues/15) and
      was removed from CRAN. Support for this has been removed in favor of
      `Lrnr_density_semiparametric` and `Lrnr_haldensify`, both of which more
      reliably provide CDE support.

# sl3 1.3.7
* Sampling methods for Monte Carlo integration and related procedures.
* A metalearner for the cross-validation selector (discrete super learner).
* A learner for bounding, including support for bounded losses.
* Resolution of a number of older issues (see #264).
* Relaxation of checks inside `Stack` objects for time series learners.
* Addition of a learner property table to `README.Rmd`.
* Maintenance and documentation updates.

# sl3 1.3.5
* Overhaul of data preprocessing.
* New screening methods and convex combination in `Lrnr_nnls`.
* Bug fixes, including covariate subsetting and better handling of `NA`s.
* Package and documentation cleanup; continuous integration and testing fixes.
* Reproducibility updates (including new versioning and DOI minting).

# sl3 1.3.0
* Fixes incorrect handling of missingness in the automatic imputation procedure.
* Adds new standard learners, including from the `gam` and `caret` packages.
* Adds custom learners for conditional density estimation, including
  semiparametric methods based on conditional mean and conditional mean/variance
  estimation as well as generalized functionality for density estimation via a
  pooled hazards approach.

# sl3 1.2.0
* Default metalearners based on task outcome types.
* Handling of imputation internally in task objects.
* Addition of several new learners, including from the `gbm`, `earth`,
  `polspline` packages.
* Fixing errors in existing learners (e.g., subtle parallelization in `xgboost`
  and `ranger`).
* Support for multivariate outcomes
* Sets default cross-validation to be revere-style.
* Support for cross-validated super learner and variable importance.

# sl3 1.1.0
* A full-featured and stable release of the project.
* Numerous learners are included and many bugs have been fixed relative to
  earlier versions (esp v1.0.0) of the software.

# sl3 1.0.0
* An initial stable release.
