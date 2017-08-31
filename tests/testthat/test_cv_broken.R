library(sl3)
library(R6)

data(cpp)
cpp <- cpp[!is.na(cpp[, "haz"]), ]
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
cpp[is.na(cpp)] <- 0
outcome <- "haz"
task <- sl3_Task$new(cpp, covariates = covars, outcome = outcome)


Lrnr_broken <- R6Class(classname = "Lrnr_broken", inherit = Lrnr_base, portable = TRUE, 
                       public=list(),
                       private=list(
                         .train=function(task){
                           stop("this task always returns an error")
                         }))

broken_learner <- Lrnr_broken$new()

broken_cv <- Lrnr_cv$new(broken_learner)

broken_fit <- broken_cv$train(task)
errors <- broken_fit$fit_object$errors$error
lapply(errors, attr, "condition")
