library(sl3)
library(devtools)
setwd("~/Dropbox/gates/sl3")
load_all()
data(cpp)
cpp <- cpp[!is.na(cpp[, "haz"]), ]
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")



# todo: make a learner (or whatever) that preprocesses data, including
# factorizing 'discretish' variables, and makes missingness indicators
cpp[is.na(cpp)] <- 0
outcome <- "haz"


task <- sl3_Task$new(cpp, covariates = covars, outcome = outcome)

data <- task$X
test <- data[, lapply(.SD, as.factor)]
z <- model.matrix(~. - 1, test)
