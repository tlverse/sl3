# sl3 1.4.3
* Additional arguments for 'Keras' learners `Lrnr_lstm_keras` and 
  `Lrnr_gru_keras` provide support for callback functions list and 2-layer 
  networks. Default `callbacks` list provides early stopping criteria with 
  respect to 'Keras' defaults and `patience` of 10 epochs.

# sl3 1.4.2
* Updates to variable importance functionality, including calculation of risk
  ratio and risk differences under covariate deletion or permutation.
* Addition of a `importance_plot` to summarize variable importance findings.
* Additions of new methods `reparameterize` and `retrain` to `Lrnr_base`, which
  allows modification of the covariate set while training on a conserved task
  and prediction on a new task using previously trained learners, respectively.

# sl3 1.4.1
* [TODO]

# sl3 1.4.0
* [TODO]

# sl3 1.3.9
* [TODO]

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
