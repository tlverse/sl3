#' @import R6
#' @import data.table
#' @import delayed
#' @importFrom methods is
#
NULL

#' Undocumented Learner
#'
#' We haven't documented this one yet. Feel free to contribute!
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#'
#' @keywords data
#'
#' @return \code{\link{Lrnr_base}} object with methods for training and
#'  prediction.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @field params A list of parameters needed to fully specify the learner. This
#'  includes things like model hyperparameters.
#'
#' @family Learners
#'
#' @name undocumented_learner
#
NULL

#' Subset of growth data from the collaborative perinatal project (CPP)
#'
#' @description
#' Subset of growth data from the collaborative perinatal project (CPP).
#' \code{cpp_imputed} drops observations for which the \code{haz} column is
#' \code{NA}, and imputes all other observations as 0. This is only for the
#' purposes of simplifying testing and examples.
#'
#' @format A data frame with 1,912 repated-measures observations and 500 unique
#'  subjects:
#'
#' \describe{
#'   \item{subjid}{Subject ID}
#'   \item{agedays}{Age since birth at examination (days)}
#'   \item{wtkg}{Weight (kg)}
#'   \item{htcm}{Standing height (cm)}
#'   \item{lencm}{Recumbent length (cm)}
#'   \item{bmi}{BMI (kg/m**2)}
#'   \item{waz}{Weight for age z-score}
#'   \item{haz}{Length/height for age z-score}
#'   \item{whz}{Weight for length/height z-score}
#'   \item{baz}{BMI for age z-score}
#'   \item{siteid}{Investigational Site ID}
#'   \item{sexn}{Sex (num)}
#'   \item{sex}{Sex}
#'   \item{feedingn}{Maternal breastfeeding status (num)}
#'   \item{feeding}{Maternal breastfeeding status}
#'   \item{gagebrth}{Gestational age at birth (days)}
#'   \item{birthwt}{Birth weight (gm)}
#'   \item{birthlen}{Birth length (cm)}
#'   \item{apgar1}{APGAR Score 1 min after birth}
#'   \item{apgar5}{APGAR Score 5 min after birth}
#'   \item{mage}{Maternal age at birth of child (yrs)}
#'   \item{mracen}{Maternal race (num)}
#'   \item{mrace}{Maternal race}
#'   \item{mmaritn}{Mothers marital status (num)}
#'   \item{mmarit}{Mothers marital status}
#'   \item{meducyrs}{Mother, years of education}
#'   \item{sesn}{Socio-economic status (num)}
#'   \item{ses}{Socio-economic status}
#'   \item{parity}{Maternal parity}
#'   \item{gravida}{Maternal num pregnancies}
#'   \item{smoked}{Maternal smoking status}
#'   \item{mcignum}{Num cigarettes mom smoked per day}
#'   \item{comprisk}{Maternal risk factors}
#' }
#'
#' @name cpp
#'
#' @docType data
#'
#' @source
#' \url{https://catalog.archives.gov/id/606622}
#'
#' Broman, Sarah. 'The collaborative perinatal project: an overview.' Handbook
#' of longitudinal research 1 (1984): 185-227.
#'
#' @usage data(cpp)
#' @usage data(cpp_imputed)
#'
#' @keywords data
#'
#' @examples
#' data(cpp)
#' head(cpp)
#
NULL

#' @name cpp_imputed
#' @rdname cpp
#
NULL

#' Subset of growth data from the collaborative perinatal project (CPP)
#'
#' @name cpp_1yr
#'
#' @docType data
#'
#' @description
#' Subset of growth data from the collaborative perinatal project (CPP) at
#' single time-point. The rows in original \code{cpp} data were subset for
#' agedays==366. See \code{?cpp} for the description of the variables.
#'
#' @source
#' \url{https://catalog.archives.gov/id/606622}
#'
#' Broman, Sarah. 'The collaborative perinatal project: an overview.' Handbook
#' of longitudinal research 1 (1984): 185-227.
#'
#' @usage data(cpp_1yr)
#'
#' @keywords data
#'
#' @examples
#' data(cpp_1yr)
#' head(cpp_1yr)
#' table(cpp_1yr[["agedays"]])
#
NULL

#' Bicycle sharing time series dataset
#'
#' @name bsds
#'
#' @docType data
#'
#' @description
#' Bicycle sharing time series dataset from the UCI Machine Learning Repository.
#'
#' @source
#' \url{https://archive.ics.uci.edu/ml/datasets/bike+sharing+dataset}
#'
#' Fanaee-T, Hadi, and Gama, Joao, 'Event labeling combining ensemble detectors
#' and background knowledge', Progress in Artificial Intelligence (2013): pp.
#' 1-15, Springer Berlin Heidelberg
#'
#' @usage data(bsds)
#'
#' @keywords data
#'
#' @examples
#' data(bsds)
#' head(bsds)
#
NULL

#' Simulated data with continuous exposure
#'
#' @name density_dat
#'
#' @docType data
#'
#' @description
#' Simulated data with continuous exposure, used with examples of conditional
#' density estimation.
#'
#' @usage data(density_dat)
#'
#' @keywords data
#'
#' @examples
#' data(density_dat)
#' head(density_dat)
#
NULL

