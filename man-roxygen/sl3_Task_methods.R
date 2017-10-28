##' @section Methods:
##'
##' \describe{
##' \item{\code{new}}{
##'   Constructs a \code{\link{sl3_Task}}
##'
##'   \emph{Usage:}
##'   \code{sl3_Task$new(data, covariates, outcome = NULL, outcome_type = NULL, outcome_levels = NULL,
##'                      id = NULL, weights = NULL, offset = NULL, nodes = NULL, column_names = NULL,
##'                      row_index = NULL, folds = NULL)}
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{data}: A \code{data.frame} or \code{data.table} containing the underlying data
##'     }
##'
##'     \item{\code{covariates}: A character vector of variable names that define the set of covariates
##'     }
##'
##'     \item{\code{outcome}: A character vector of variable names that define the set of outcomes. Usually just one variable, although some learners support multivariate outcomes. Use \code{sl3_list_learners("multivariate_outcome")} to find such learners.
##'     }
##'
##'     \item{\code{outcome_type}: A \code{Variable_type} object that defines the variable type of the outcome. Alternatively, a character specifying such a type. See \code{\link{variable_type}} for details on defining variable types.
##'     }
##'
##'     \item{\code{outcome_levels}: A vector of levels expected for the outcome variable. If \code{outcome_type} is a character, this will be used to construct an appropriate \code{\link{variable_type}} object.
##'     }
##'     \item{\code{id}: A character indicating which variable (if any) to be used as an observation "id", for learners that support clustered observations. Use \code{sl3_list_learners("id")} to find such learners.
##'     }
##'     \item{\code{weights}: A character indicating which variable (if any) to be used as observation weights, for learners that support that. Use \code{sl3_list_learners("weights")} to find such learners.
##'     }
##'     \item{\code{offset}: A character indicating which variable (if any) to be used as an observation "id", for methods that support clustered observations.  Use \code{sl3_list_learners("offset")} to find such learners.
##'     }
##'     \item{\code{nodes}: A list of character vectors as nodes. This will override the \code{covariates}, \code{outcome}, \code{id}, \code{weights}, and \code{offset} arguments if specified, and is an alternative way to specify those arguments.
##'     }
##'     \item{\code{column_names}: A named list of characters that maps between column names in \code{data} and how those variables are referenced in \code{sl3_Task} functions. 
##'     }
##'     }
##'   }
##' }
