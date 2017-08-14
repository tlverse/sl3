
<!-- README.md is generated from README.Rmd. Please edit that file -->
R/`sl3`: new and improved Super Learning
========================================

[![Travis-CI Build Status](https://travis-ci.org/jeremyrcoyle/sl3.svg?branch=master)](https://travis-ci.org/jeremyrcoyle/sl3) [![Build status](https://ci.appveyor.com/api/projects/status/25reu5wdhrwj9qgy?svg=true)](https://ci.appveyor.com/project/jeremyrcoyle/sl3) [![Coverage Status](https://img.shields.io/codecov/c/github/jeremyrcoyle/sl3/master.svg)](https://codecov.io/github/jeremyrcoyle/sl3?branch=master) [![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) [![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0) [![Join the chat at https://gitter.im/sl3-Rpkg/Lobby](https://badges.gitter.im/sl3-Rpkg/Lobby.svg)](https://gitter.im/sl3-Rpkg/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

> A modern implementation of the Super Learner ensemble learning / stacking algorithm

**Authors:** [Jeremy Coyle](https://github.com/jeremyrcoyle), [Nima Hejazi](https://github.com/nhejazi), [Ivana Malenica](https://github.com/podTockom), [Oleg Sofrygin](https://github.com/osofr)

------------------------------------------------------------------------

What's `sl3`?
-------------

`sl3` is a modern implementation of the Super Learner algorithm of van der Laan, Polley, and Hubbard (2007). The Super Learner algorithm performs ensemble learning in one of two fashions:

1.  The "discrete" Super Learner can be used to select the best prediction algorithm among a supplied library of learning algorithms ("learners" in the `sl3` nomenclature) -- that is, that algorithm which minimizes the cross-validated risk with respect to some appropriate loss function.
2.  The "ensemble" Super Learner can be used to assign weights to specified learning algorithms (in a user-supplied library) in order to create a combination of these learners that minimizes the cross-validated risk with respect to an appropriate loss function. This notion of weighted combinations has also been called *stacked regression* (Breiman 1996).

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
devtools::install_github("jeremyrcoyle/sl3")
```

<!--
To contribute, install the _development version_ from GitHub via
[`devtools`](https://www.rstudio.com/products/rpackages/devtools/):


```r
devtools::install_github("jeremyrcoyle/sl3", ref = "develop")
```
-->

------------------------------------------------------------------------

Contributions and Issues
------------------------

It is our hope that `sl3` will grow to be widely used for creating stacked regression models and the cross-validation of pipelines that make up such models, as well as the variety of other applications in which the Super Learner algorithm plays a role. To that end, contributions are very welcome, though we ask that interested contributors consult our [`contribution guidelines`](https://github.com/jeremyrcoyle/sl3/blob/master/CONTRIBUTING.md) prior to submitting a pull request.

If you encounter any bugs or have any specific feature requests, please [file an issue](https://github.com/jeremyrcoyle/sl3/issues).

[![](https://graphs.waffle.io/jeremyrcoyle/sl3/throughput.svg)](https://waffle.io/jeremyrcoyle/sl3/metrics/throughput)

------------------------------------------------------------------------

Examples
--------

`sl3` makes the process of applying screening algorithms, learning algorithms, combining both types of algorithms into a stacked regression model, and cross-validating this whole process essentially trivial. The best way to understand this is to see the `sl3` package in action:

``` r
set.seed(49753)
suppressMessages(library(data.table))
library(dplyr)
library(SuperLearner)
#> Loading required package: nnls
#> Super Learner
#> Version: 2.0-23-9000
#> Package created on 2017-07-20
library(origami)
#> origami: Generalized Cross-Validation Framework
#> Version: 0.8.0
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
#> 
#> Attaching package: 'Matrix'
#> The following object is masked from 'package:tidyr':
#> 
#>     expand
#> Loading required package: foreach
#> 
#> Attaching package: 'foreach'
#> The following objects are masked from 'package:purrr':
#> 
#>     accumulate, when
#> Loaded glmnet 2.0-10
preds <- stack_fit$predict()
head(preds)
#>      Lrnr_pkg_SuperLearner_SL.glmnet   Lrnr_glm
#> [1,]                       0.3525946 0.36298498
#> [2,]                       0.3525946 0.36298498
#> [3,]                       0.2442593 0.25993072
#> [4,]                       0.2442593 0.25993072
#> [5,]                       0.2442593 0.25993072
#> [6,]                       0.0269504 0.05680264
#>      Lrnr_pkg_SuperLearner_screener_screen.glmnet___Lrnr_glm
#> [1,]                                              0.36228209
#> [2,]                                              0.36228209
#> [3,]                                              0.25870995
#> [4,]                                              0.25870995
#> [5,]                                              0.25870995
#> [6,]                                              0.05600958
```

------------------------------------------------------------------------

License
-------

© 2017 [Jeremy R. Coyle](https://github.com/jeremyrcoyle), [Nima S. Hejazi](https://github.com/nhejazi), [Ivana Malenica](https://github.com/podTockom), [Oleg Sofrygin](https://github.com/osofr)

The contents of this repository are distributed under the GPL-3 license. See file `LICENSE` for details.

------------------------------------------------------------------------

References
----------

Breiman, Leo. 1996. “Stacked Regressions.” *Machine Learning* 24 (1). Springer: 49–64.

van der Laan, Mark J., Eric C. Polley, and Alan E. Hubbard. 2007. “Super Learner.” *Statistical Applications in Genetics and Molecular Biology* 6 (1).
