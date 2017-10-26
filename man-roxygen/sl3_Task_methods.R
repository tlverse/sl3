##' @section Methods:
##'
##' \describe{
##' \item{\code{new}}{
##'   Constructs a \code{\link{sl3_Task}}
##'
##'   \emph{Usage:}
##'   \code{sl3_Task$new((data, covariates, outcome = NULL, outcome_type = NULL, outcome_levels = NULL,
##'                       id = NULL, weights = NULL, offset = NULL, nodes = NULL, column_names = NULL,
##'                       row_index = NULL, folds = NULL))}
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{data}: A \code{data.frame} or \code{data.table} containing the underlying data
##'     }
##'
##'     \item{\code{covariates}: A character vector of variable names that define the set of covariates
##'     }
##'
##'     \item{\code{outcome}: A character vector of variable names that define the set of outcomes. Usually just one variable, although some learners support multivariate outcomes. Use \code{sl3_list_learners("multivariate_outcome")} for a list.
##'     }
##'
##'     \item{\code{outcome_type}: A \code{\link{Variable_type}} object that defines the variable type of the outcome. Alternatively, a character specifying such a type. See \code{\link{variable_type}} for details on defining variable types.
##'     }
##'
##'     }
##'   }
##' }
