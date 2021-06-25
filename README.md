
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R/`sl3`: Super Machine Learning with Pipelines

[![Travis-CI Build
Status](https://travis-ci.com/tlverse/sl3.svg?branch=master)](https://travis-ci.com/tlverse/sl3)
[![Appveyor Build
Status](https://ci.appveyor.com/api/projects/status/hagh8vidrdeacr7f?svg=true)](https://ci.appveyor.com/project/tlverse/sl3)
[![Coverage
Status](https://img.shields.io/codecov/c/github/tlverse/sl3/master.svg)](https://codecov.io/github/tlverse/sl3?branch=master)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1342293.svg)](https://doi.org/10.5281/zenodo.1342293)

> A flexible implementation of the Super Learner ensemble learning
> system

**Authors:** [Jeremy Coyle](https://github.com/jeremyrcoyle), [Nima
Hejazi](https://nimahejazi.org), [Ivana
Malenica](https://github.com/imalenica), [Rachael
Phillips](https://github.com/rachaelvp), and [Oleg
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
    minimizes the cross-validated risk.
2.  The *ensemble* Super Learner can be used to assign weights to a set
    of specified learning algorithms (from a user-supplied library of
    such algorithms) so as to create a combination of these learners
    that minimizes the cross-validated risk. This notion of weighted
    combinations has also been referred to as *stacked regression*
    (Breiman 1996) and *stacked generalization* (Wolpert 1992).

Looking for long-form documentation or a walkthrough of the `sl3`
package? Don’t worry\! Just take a look at [the chapter in our
book](https://tlverse.org/tlverse-handbook/sl3.html).

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
remotes::install_github("tlverse/sl3@v1.3.7")
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
task <- sl3_Task$new(
  data = cpp,
  covariates = covars,
  outcome = "haz"
)

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
#> 1:                      0.35259458    0.36298498
#> 2:                      0.35259458    0.36298498
#> 3:                      0.24425929    0.25993072
#> 4:                      0.24425929    0.25993072
#> 5:                      0.24425929    0.25993072
#> 6:                      0.02695039    0.05680264
#>    Pipeline(Lrnr_pkg_SuperLearner_screener_screen.glmnet->Lrnr_glm_TRUE)
#> 1:                                                            0.36228209
#> 2:                                                            0.36228209
#> 3:                                                            0.25870995
#> 4:                                                            0.25870995
#> 5:                                                            0.25870995
#> 6:                                                            0.05600958
```

### Parallelization with `future`s

While it’s straightforward to fit a stack of learners (as above), it’s
easy to take advantage of `sl3`’s built-in parallelization support too.
To do this, you can simply choose a `plan()` from the [`future`
ecosystem](https://CRAN.R-project.org/package=future).

``` r
# let's load the future package and set 4 cores for parallelization
library(future)
plan(multisession, workers = 4L)

# now, let's re-train our Stack in parallel
stack_fit <- learner_stack$train(task)
preds <- stack_fit$predict()
```

### Controlling the number of CV folds

In the above examples, we fit stacks of learners, but didn’t create a
Super Learner ensemble, which uses cross-validation (CV) to build the
ensemble model. For the sake of computational expedience, we may be
interested in lowering the number of CV folds (from 10). Let’s take a
look at how to do both below.

``` r
# first, let's instantiate some more learners and create a Super Learner
mean_learner <- Lrnr_mean$new()
rf_learner <- Lrnr_ranger$new()
sl <- Lrnr_sl$new(mean_learner, glm_learner, rf_learner)

# CV folds are controlled in the sl3_Task object; we can lower the number of
# folds simply by specifying this in creating the Task
task <- sl3_Task$new(
  data = cpp,
  covariates = covars,
  outcome = "haz",
  folds = 5L
)

# now, let's fit the Super Learner with just 5-fold CV, then get predictions
sl_fit <- sl$train(task)
sl_preds <- sl_fit$predict()
```

The `folds` argument to `sl3_Task` supports both integers (for V-fold
CV) and all of the CV schemes supported in the [`origami`
package](https://CRAN.R-project.org/package=origami). To see the full
list, query `?fold_funs` from within `R` or take a look at [`origami`’s
online documentation](https://tlverse.org/origami/reference/).

-----

## Learner Properties

Properties supported by `sl3` learners are presented in the following
table:

<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:200px; overflow-x: scroll; width:100%; ">

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">

binomial

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">

categorical

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">

continuous

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">

cv

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">

density

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">

h2o

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">

ids

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">

importance

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">

offset

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">

preprocessing

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">

sampling

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">

screener

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">

timeseries

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">

weights

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">

wrapper

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Lrnr\_arima

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_bartMachine

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_bayesglm

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_bilstm

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_bound

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_caret

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_cv

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_cv\_selector

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_dbarts

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_define\_interactions

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_density\_discretize

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_density\_hse

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_density\_semiparametric

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_earth

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_expSmooth

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_gam

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_gbm

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_glm

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_glm\_fast

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_glmnet

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_grf

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_gru\_keras

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_gts

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_h2o\_glm

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_h2o\_grid

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_hal9001

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_haldensify

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_HarmonicReg

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_hts

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_independent\_binomial

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_lightgbm

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_lstm

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_lstm\_keras

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_mean

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_multiple\_ts

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_multivariate

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_nnet

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_nnls

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_optim

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_pca

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_pkg\_SuperLearner

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_pkg\_SuperLearner\_method

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_pkg\_SuperLearner\_screener

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_polspline

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_pooled\_hazards

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_randomForest

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_ranger

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_revere\_task

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_rpart

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_rugarch

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_screener\_augment

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_screener\_coefs

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_screener\_correlation

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_screener\_importance

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_sl

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_solnp

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_solnp\_density

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_stratified

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_subset\_covariates

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_svm

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_ts\_weights

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_tsDyn

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

</tr>

<tr>

<td style="text-align:left;">

Lrnr\_xgboost

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

x

</td>

<td style="text-align:left;">

√

</td>

<td style="text-align:left;">

x

</td>

</tr>

</tbody>

</table>

</div>

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
    @software{coyle2021sl3-rpkg,
      author = {Coyle, Jeremy R and Hejazi, Nima S and Malenica, Ivana and 
        Phillips, Rachael V and Sofrygin, Oleg},
      title = {{sl3}: Modern Pipelines for Machine Learning and {Super
        Learning}},
      year = {2021},
      howpublished = {\url{https://github.com/tlverse/sl3}},
      note = {{R} package version 1.4.2},
      url = {https://doi.org/10.5281/zenodo.1342293},
      doi = {10.5281/zenodo.1342293}
    }
```

-----

## License

© 2017-2021 [Jeremy R. Coyle](https://github.com/jeremyrcoyle), [Nima S.
Hejazi](https://nimahejazi.org), [Ivana
Malenica](https://github.com/podTockom), [Rachael V.
Phillips](https://github.com/rachaelvp), [Oleg
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

van der Laan, Mark J, Eric C Polley, and Alan E Hubbard. 2007. “Super
Learner.” *Statistical Applications in Genetics and Molecular Biology* 6
(1).

</div>

<div id="ref-wolpert1992stacked">

Wolpert, David H. 1992. “Stacked Generalization.” *Neural Networks* 5
(2): 241–59.

</div>

</div>
