## Allows us to use any learner in sl3 as a bin learner in condensier
## This additional injection works when Lrnr_condensier$new(bin_estimator =
## Lrnr_base) is a learner from sl3 package.
##
## In this, the sl3 learner 'Lrnr_base' object is injected into the wraper class
## below. This R6 object then provides a communication link between the two
## packages (sl3 <-> condensier).

#' sl3 Learner wrapper for condensier
#'
#' This wrapper allows the use of any \code{sl3} Learner as a Learner for
#' \code{condensier}. For details, see the \code{\link[condensier]{fit_density}}
#' function.
#'
#' @docType class
#'
#' @export
#
Lrnr_pkg_condensier_logisfitR6 <- R6Class("Lrnr_pkg_condensier_logisfitR6",
                                          inherit = condensier::logisfitR6,
  public = list(
    lmclass = NULL,
    fitfunname = NULL,

    initialize = function(sl3_lrnr, ...) {
      assert_that(is(sl3_lrnr,"Lrnr_base"))
      self$lmclass <- paste0("sl3::", class(sl3_lrnr)[1L])
      self$fitfunname <- paste0("sl3::", class(sl3_lrnr)[1L], "$train")
      private$sl3_lrnr <- sl3_lrnr
      self$get_validity
      super$initialize(...)
    },

    fit = function(datsum_obj) {
      verbose <- getOption("sl3.verbose")
      if (verbose) print(paste("calling ", self$fitfunname))
      X_mat <- datsum_obj$getXDT
      Y_vals <- datsum_obj$getY
      dataDT <- cbind(X_mat, Y = Y_vals)
      sl3_lrnr <- private$sl3_lrnr
      if (nrow(dataDT) > 0) {
        task <- sl3_Task$new(dataDT, covariates = colnames(X_mat),
                             outcome = colnames(dataDT)[ncol(dataDT)])
        out <- capture.output(
          sl3_lrnr <- try(suppressWarnings(sl3_lrnr$train(task)))
        )
        if (inherits(sl3_lrnr, "try-error") ||
            inherits(sl3_lrnr$fit_object, "try-error")) {
          # if failed, use fall back learner
          if (verbose) {
            message(paste0("learner ", self$fitfunname,
                           " failed, trying private$fallback_learner; "))
            if (inherits(sl3_lrnr, "Lrnr_base")) {
              print(sl3_lrnr$name)
            } else {
              print(sl3_lrnr)
            }
          }
          sl3_lrnr <- private$fallback_learner$new(family =
                                                   "binomial")$train(task)
        }
      }
      if (verbose) print(sl3_lrnr)
      return(sl3_lrnr)
    },

    predict = function(datsum_obj, m.fit) {
      verbose <- getOption("sl3.verbose")
      X_mat <- datsum_obj$getXDT
      assert_that(!is.null(X_mat)); assert_that(!is.null(datsum_obj$subset_idx))
      pAout <- rep.int(NA_real_, datsum_obj$n)
      if (sum(datsum_obj$subset_idx > 0)) {
        new_task <- sl3_Task$new(X_mat, covariates = colnames(X_mat),
                                 outcome = NULL)
        ## Learner hasn't been trained,
        ## means we are making predictionsin for a last degenerate bin.
        ## These predictions play no role and aren't used.
        if (is(m.fit, "Lrnr_base") && !m.fit$is_trained) {
          pAout[datsum_obj$subset_idx] <- 0.5
        } else {
          pAout[datsum_obj$subset_idx] <- m.fit$predict(new_task)[[1]]
        }
      }
      return(pAout)
    }
  ),
  private = list(
    fallback_learner = Lrnr_glm_fast,
    sl3_lrnr = NULL
  )
)

