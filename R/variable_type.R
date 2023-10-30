#' @rdname variable_type
#'
#' @export
Variable_Type <- R6Class(
  classname = "Variable_Type",
  portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(type = NULL, levels = NULL, bounds = NULL, x = NULL,
                          pcontinuous = getOption("sl3.pcontinuous")) {
      detect_type <- function(flag) {
        if (is.null(x)) {
          if (flag) {
            stop("type not specified, and no x from which to infer it.")
          } else {
            type <- "none"
          }
        } else {
          nunique <- length(na.exclude(unique(x)))
          if (!is.null(ncol(x))) {
            type <- "multivariate"
          } else if (nunique == 1) {
            type <- "constant"
          } else if (nunique == 2) {
            type <- "binomial"
          } else if ((is.factor(x)) || (((nunique / length(x)) < pcontinuous) &&
            (nunique < 20))) {
            type <- "categorical"
          } else {
            type <- "continuous"
          }
        }
        return(type)
      }

      if (is.null(type)) {
        type <- detect_type(flag = TRUE)
      } else {
        detected_type <- detect_type(flag = FALSE)
        if (detected_type != type & detected_type != "none") {
          if (type == "quasibinomial") {
            if (min(x) < 0 | max(x) > 1) {
              stop(paste0(
                "Outcome values fall outside [0,1]! Supplied outcome_type (",
                type, ") cannot support outcome values outside [0,1]."
              ))
            }
          } else {
            warning(paste0(
              "The supplied outcome_type (", type,
              ") does not correspond to the detected type (", detected_type,
              "). Ensure outcome_type is specified appropriately."
            ))
          }
        }
      }
      private$.type <- type
      if (type %in% c("binomial", "categorical") && is.null(levels)) {
        if (!is.null(x)) {
          levels <- get_levels(x)
        } else if (type == "binomial") {
          levels <- c(0, 1)
        } else if (is.null(levels)) {
          stop(sprintf("levels or x must be specified for %s", type))
        }
      }
      private$.levels <- levels
      if (type == "quasibinomial" && is.null(bounds)) {
        bounds <- c(0, 1)
      }
      private$.bounds <- bounds
    },
    print = function() {
      print(self$type)
      print(self$levels)
      print(self$bounds)
    },
    glm_family = function(return_object = FALSE) {
      type <- self$type
      family <- switch(type,
        continuous = "gaussian",
        binomial = "binomial",
        quasibinomial = "quasibinomial",
        categorical = "multinomial",
        constant = "binomial",
        "unknown"
      )
      if (family == "unknown") {
        warning("No family for this outcome_type. Defaulting to gaussian")
        family <- "gaussian"
      }
      if (return_object) {
        family_fun <- try({
          get(
            family,
            mode = "function",
            envir = parent.frame()
          )
        })
        if (inherits(family_fun, "try-error")) {
          stop(paste(
            "Family object requested for family that does not have",
            "a generator.\n You're probably using an unsupported",
            "learner/outcome_type combination. Specify family",
            "manually."
          ))
        } else {
          family <- family_fun()
        }
      }
      return(family)
    },
    format = function(x) {
      if (self$type == "binomial") {
        max_level <- max(self$levels)
        formatted <- as.numeric(x == max_level)
      } else if (self$type == "quasibinomial") {
        if (any(x < 0) || any(x > 1)) {
          warning(paste(
            "Detected 'Y' outside [0-1] range with 'quasibinomial'",
            "outcome_type -- beware, this will break most",
            "learners."
          ))
        }
        formatted <- x
      } else if (self$type == "categorical") {
        formatted <- factor(x, levels = self$levels)
      } else {
        formatted <- x
      }
      return(formatted)
    }
  ),
  active = list(
    type = function() {
      return(private$.type)
    },
    levels = function() {
      return(private$.levels)
    },
    bounds = function() {
      return(private$.bounds)
    }
  ),
  private = list(
    .type = NULL,
    .levels = NULL,
    .bounds = NULL
  )
)

#' Specify Variable Type
#'
#' @param type A type name. Valid choices include "binomial", "categorical",
#'  "continuous", and "multivariate". When not specified, this is inferred.
#' @param levels Valid levels for discrete types.
#' @param bounds Bounds for continuous variables.
#' @param x Data to use for inferring type if not specified.
#' @param pcontinuous If \code{type} above is inferred, the proportion of
#'  unique observations above which the variable is considered continuous.
#'
#' @export
variable_type <- function(type = NULL, levels = NULL, bounds = NULL, x = NULL,
                          pcontinuous = getOption("sl3.pcontinuous")) {
  return(Variable_Type$new(
    type = type, levels = levels, bounds = bounds, x = x,
    pcontinuous = pcontinuous
  ))
}
