
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R/`sl3`: modern Super Learning with pipelines

[![Travis-CI Build
Status](https://travis-ci.com/tlverse/sl3.svg?branch=master)](https://travis-ci.com/tlverse/sl3)
[![Appveyor Build
Status](https://ci.appveyor.com/api/projects/status/hagh8vidrdeacr7f?svg=true)](https://ci.appveyor.com/project/tlverse/sl3)
[![Coverage
Status](https://img.shields.io/codecov/c/github/tlverse/sl3/master.svg)](https://codecov.io/github/tlverse/sl3?branch=master)
[![CRAN](http://www.r-pkg.org/badges/version/sl3)](http://www.r-pkg.org/pkg/sl3)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/sl3)](https://CRAN.R-project.org/package=sl3)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3558317.svg)](https://doi.org/10.5281/zenodo.3558317)

> A modern implementation of the Super Learner algorithm for ensemble
> learning and model stacking

**Authors:** [Jeremy Coyle](https://github.com/jeremyrcoyle), [Nima
Hejazi](https://nimahejazi.org), [Ivana
Malenica](https://github.com/podTockom), [Oleg
Sofrygin](https://github.com/osofr)

-----

## What’s `sl3`?

`sl3` is a modern implementation of the Super Learner algorithm of van
der Laan, Polley, and Hubbard (2007). The Super Learner algorithm
performs ensemble learning in one of two fashions:

1.  The *discrete* Super Learner can be used to select the best
    prediction algorithm from among a supplied library of machine
    learning algorithms (“learners” in the `sl3` nomenclature) – that
    is, the discrete Super Learner is the single learning algorithm that
    minimizes the cross-validated risk with respect to an appropriate
    loss function.
2.  The *ensemble* Super Learner can be used to assign weights to a set
    of specified learning algorithms (from a user-supplied library of
    such algorithms) so as to create a combination of these learners
    that minimizes the cross-validated risk with respect to an
    appropriate loss function. This notion of weighted combinations has
    also been referred to as *stacked regression* (Breiman 1996) and
    *stacked generalization* (Wolpert 1992).

-----

## Installation

<!--
For standard use, we recommend installing the package from
[CRAN](https://cran.r-project.org/) via


```r
install.packages("sl3")
```
-->

Install the *most recent version* from the `master` branch on GitHub via
[`remotes`](https://CRAN.R-project.org/package=remotes):

``` r
remotes::install_github("tlverse/sl3")
```

Past stable releases may be located via the
[releases](https://github.com/tlverse/sl3/releases) page on GitHub and
may be installed by including the appropriate major version tag. For
example,

``` r
remotes::install_github("tlverse/sl3@v1.3.5")
```

To contribute, check out the `devel` branch and consider submitting a
pull request.

-----

## Issues

If you encounter any bugs or have any specific feature requests, please
[file an issue](https://github.com/tlverse/sl3/issues).

-----

## Examples

`sl3` makes the process of applying screening algorithms, learning
algorithms, combining both types of algorithms into a stacked regression
model, and cross-validating this whole process essentially trivial. The
best way to understand this is to see the `sl3` package in action:

``` r
set.seed(49753)
library(tidyverse)
library(data.table)
library(SuperLearner)
library(origami)
library(sl3)

# load example data set
data(cpp)
cpp <- cpp %>%
  dplyr::filter(!is.na(haz)) %>%
  mutate_all(~ replace(., is.na(.), 0))

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
preds <- stack_fit$predict()
head(preds)
#>    Lrnr_pkg_SuperLearner_SL.glmnet Lrnr_glm_TRUE
#> 1:                      0.35618966    0.36298498
#> 2:                      0.35618966    0.36298498
#> 3:                      0.24964615    0.25993072
#> 4:                      0.24964615    0.25993072
#> 5:                      0.24964615    0.25993072
#> 6:                      0.03776486    0.05680264
#>    Pipeline(Lrnr_pkg_SuperLearner_screener_screen.glmnet->Lrnr_glm_TRUE)
#> 1:                                                            0.36228209
#> 2:                                                            0.36228209
#> 3:                                                            0.25870995
#> 4:                                                            0.25870995
#> 5:                                                            0.25870995
#> 6:                                                            0.05600958
```

-----

## Contributions

Contributions are very welcome. Interested contributors should consult
our [contribution
guidelines](https://github.com/tlverse/sl3/blob/master/CONTRIBUTING.md)
prior to submitting a pull request.

-----

## Citation

After using the `sl3` R package, please cite the following:

``` 
    @manual{coyle2019sl3,
      author = {Coyle, Jeremy R and Hejazi, Nima S and Malenica, Ivana and
        Sofrygin, Oleg},
      title = {{sl3}: Modern Pipelines for Machine Learning and {Super
        Learning}},
      year = {2019},
      howpublished = {\url{https://github.com/tlverse/sl3}},
      note = {{R} package version 1.3.5},
      url = {https://doi.org/10.5281/zenodo.3558317},
      doi = {10.5281/zenodo.3558317}
    }
```

-----

## License

© 2017-2019 [Jeremy R. Coyle](https://github.com/jeremyrcoyle), [Nima S.
Hejazi](https://nimahejazi.org), [Ivana
Malenica](https://github.com/podTockom), [Oleg
Sofrygin](https://github.com/osofr)

The contents of this repository are distributed under the GPL-3 license.
See file `LICENSE` for details.

-----

## References

<div id="refs" class="references hanging-indent">

<div id="ref-breiman1996stacked">

Breiman, Leo. 1996. “Stacked Regressions.” *Machine Learning* 24 (1):
49–64.

</div>

<div id="ref-vdl2007super">

van der Laan, Mark J., Eric C. Polley, and Alan E. Hubbard. 2007. “Super
Learner.” *Statistical Applications in Genetics and Molecular Biology* 6
(1).

</div>

<div id="ref-wolpert1992stacked">

Wolpert, David H. 1992. “Stacked Generalization.” *Neural Networks* 5
(2): 241–59.

</div>

</div>
