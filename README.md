
<!-- README.md is generated from README.Rmd. Please edit that file -->
R/`sl3`: modern Super Learning with pipelines
=============================================

[![Travis-CI Build Status](https://travis-ci.org/tlverse/sl3.svg?branch=master)](https://travis-ci.org/tlverse/sl3) [![Build status](https://ci.appveyor.com/api/projects/status/lfv64jnygnmx6txi?svg=true)](https://ci.appveyor.com/project/jeremyrcoyle/sl3) [![Coverage Status](https://img.shields.io/codecov/c/github/tlverse/sl3/master.svg)](https://codecov.io/github/tlverse/sl3?branch=master) [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) [![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1342294.svg)](https://doi.org/10.5281/zenodo.1342294) [![Join the chat at https://gitter.im/sl3-Rpkg/Lobby](https://badges.gitter.im/sl3-Rpkg/Lobby.svg)](https://gitter.im/sl3-Rpkg/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

> A modern implementation of the Super Learner algorithm for ensemble learning and model stacking

**Authors:** [Jeremy Coyle](https://github.com/jeremyrcoyle), [Nima Hejazi](https://github.com/nhejazi), [Ivana Malenica](https://github.com/podTockom), [Oleg Sofrygin](https://github.com/osofr)

------------------------------------------------------------------------

What's `sl3`?
-------------

`sl3` is a modern implementation of the Super Learner algorithm of van der Laan, Polley, and Hubbard (2007). The Super Learner algorithm performs ensemble learning in one of two fashions:

1.  The *discrete* Super Learner can be used to select the best prediction algorithm from among a supplied library of machine learning algorithms ("learners" in the `sl3` nomenclature) -- that is, the discrete Super Learner is the single learning algorithm that minimizes the cross-validated risk with respect to an appropriate loss function.
2.  The *ensemble* Super Learner can be used to assign weights to a set of specified learning algorithms (from a user-supplied library of such algorithms) so as to create a combination of these learners that minimizes the cross-validated risk with respect to an appropriate loss function. This notion of weighted combinations has also been referred to as *stacked regression* (Breiman 1996) and *stacked generalization* (Wolpert 1992).

------------------------------------------------------------------------

Installation
------------

<!--
For standard use, we recommend installing the package from
[CRAN](https://cran.r-project.org/) via


```r
install.packages("sl3")
```
-->
Install the most recent *stable release* from GitHub via [`devtools`](https://www.rstudio.com/products/rpackages/devtools/):

``` r
devtools::install_github("tlverse/sl3")
```

------------------------------------------------------------------------

Issues
------

If you encounter any bugs or have any specific feature requests, please [file an issue](https://github.com/tlverse/sl3/issues).

------------------------------------------------------------------------

Examples
--------

`sl3` makes the process of applying screening algorithms, learning algorithms, combining both types of algorithms into a stacked regression model, and cross-validating this whole process essentially trivial. The best way to understand this is to see the `sl3` package in action:

``` r
set.seed(49753)
suppressMessages(library(data.table))
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:data.table':
#> 
#>     between, first, last
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(SuperLearner)
#> Loading required package: nnls
#> Super Learner
#> Version: 2.0-24
#> Package created on 2018-08-10
library(origami)
#> origami: Generalized Cross-Validation Framework
#> Version: 1.0.0
library(sl3)

# load example data set
data(cpp)
cpp <- cpp %>%
  dplyr::filter(!is.na(haz)) %>%
  mutate_all(funs(replace(., is.na(.), 0)))

# use covariates of intest and the outcome to build a task object
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs",
            "sexn")
task <- sl3_Task$new(cpp, covariates = covars, outcome = "haz")

# set up screeners and learners via built-in functions and pipelines
slscreener <- Lrnr_pkg_SuperLearner_screener$new("screen.glmnet")
glm_learner <- Lrnr_glm$new()
screen_and_glm <- Pipeline$new(slscreener, glm_learner)
SL.glmnet_learner <- Lrnr_pkg_SuperLearner$new(SL_wrapper = "SL.glmnet")

# stack learners into a model (including screeners and pipelines)
learner_stack <- Stack$new(SL.glmnet_learner, glm_learner, screen_and_glm)
stack_fit <- learner_stack$train(task)
#> Loading required package: glmnet
#> Loading required package: Matrix
#> Loading required package: foreach
#> Loaded glmnet 2.0-16
preds <- stack_fit$predict()
head(preds)
#>    Lrnr_pkg_SuperLearner_SL.glmnet Lrnr_glm_TRUE
#> 1:                      0.35345519    0.36298498
#> 2:                      0.35345519    0.36298498
#> 3:                      0.24554305    0.25993072
#> 4:                      0.24554305    0.25993072
#> 5:                      0.24554305    0.25993072
#> 6:                      0.02953193    0.05680264
#>    Lrnr_pkg_SuperLearner_screener_screen.glmnet___Lrnr_glm_TRUE
#> 1:                                                   0.36228209
#> 2:                                                   0.36228209
#> 3:                                                   0.25870995
#> 4:                                                   0.25870995
#> 5:                                                   0.25870995
#> 6:                                                   0.05600958
```

------------------------------------------------------------------------

Contributions
-------------

It is our hope that `sl3` will grow to be widely used for creating stacked regression models and the cross-validation of pipelines that make up such models, as well as the variety of other applications in which the Super Learner algorithm plays a role. To that end, contributions are very welcome, though we ask that interested contributors consult our [contribution guidelines](https://github.com/jeremyrcoyle/sl3/blob/master/CONTRIBUTING.md) prior to submitting a pull request.

------------------------------------------------------------------------

After using the `sl3` R package, please cite the following:

        @misc{coyle2018sl3,
          author = {Coyle, Jeremy R and Hejazi, Nima S and Malenica, Ivana and
            Sofrygin, Oleg},
          title = {{sl3}: Modern Pipelines for Machine Learning and {Super
            Learning}},
          year  = {2018},
          howpublished = {\url{https://github.com/tlverse/sl3}},
          note = {{R} package version 1.1.0},
          url = {https://doi.org/10.5281/zenodo.1342294},
          doi = {10.5281/zenodo.1342294}
        }

------------------------------------------------------------------------

License
-------

© 2017-2018 [Jeremy R. Coyle](https://github.com/jeremyrcoyle), [Nima S. Hejazi](https://github.com/nhejazi), [Ivana Malenica](https://github.com/podTockom), [Oleg Sofrygin](https://github.com/osofr)

The contents of this repository are distributed under the GPL-3 license. See file `LICENSE` for details.

------------------------------------------------------------------------

References
----------

Breiman, Leo. 1996. “Stacked Regressions.” *Machine Learning* 24 (1). Springer: 49–64.

van der Laan, Mark J., Eric C. Polley, and Alan E. Hubbard. 2007. “Super Learner.” *Statistical Applications in Genetics and Molecular Biology* 6 (1).

Wolpert, David H. 1992. “Stacked Generalization.” *Neural Networks* 5 (2). Elsevier: 241–59.
