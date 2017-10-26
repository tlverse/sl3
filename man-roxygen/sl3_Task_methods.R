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
##'     \item{\code{use_cache}:   Use the internal cache to avoid reading or writing to the underlying storage if the data has already been seen (i.e., we have seen the hash of the object before).
##'     }
##'   }
##' }
##' \item{\code{flush_cache}}{
##'   Flush the temporary cache of objects that accumulates as the storr is used.  Should not need to be called often.
##'
##'   \emph{Usage:}
##'   \code{flush_cache()}
##' }
##' \item{\code{set}}{
##'   Set a key to a value.
##'
##'   \emph{Usage:}
##'   \code{set(key, value, namespace = self$default_namespace, use_cache = TRUE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{key}:   The key name.  Can be any string.
##'     }
##'
##'     \item{\code{value}:   Any R object to store.  The object will generally be serialized (this is not actually true for the environment storr) so only objects that would usually be expected to survive a \code{saveRDS}/\code{readRDS} roundtrip will work.  This excludes Rcpp modules objects, external pointers, etc.  But any "normal" R object will work fine.
##'     }
##'
##'     \item{\code{namespace}:   An optional namespace.  By default the default namespace that the storr was created with will be used (by default that is "objects").  Different namespaces allow different types of objects to be stored without risk of names colliding.  Use of namespaces is optional, but if used they must be a string.
##'     }
##'
##'     \item{\code{use_cache}:   Use the internal cache to avoid reading or writing to the underlying storage if the data has already been seen (i.e., we have seen the hash of the object before).
##'     }
##'   }
##'}
##'}
