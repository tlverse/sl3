# context("Test h2o grid")

# if (FALSE) {
#     setwd("..")
#     setwd("..")
#     getwd()
#     library("devtools")
#     document()
#     load_all("./")  # load all R files in /R and datasets in /data. Ignores NAMESPACE:
#     setwd("..")
#     install("sl3", build_vignettes = FALSE, dependencies = FALSE)  # INSTALL W/ devtools:
#     Sys.setenv(JAVA_HOME = "/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/")
# }


# library(testthat)
# library(sl3)
# library(h2o)
# h2o::h2o.init(nthread = 1)
# # library(data.table) library(origami)
# library(SuperLearner)
# set.seed(1)

# data(cpp)
# cpp <- cpp[!is.na(cpp[, "haz"]), ]
# covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
# cpp[is.na(cpp)] <- 0
# outcome <- "haz"

# task <- sl3_Task$new(cpp, covariates = covars, outcome = outcome)
# task$nodes$covariates

# options(sl3.verbose = TRUE)

# h2o::h2o.shutdown(prompt = FALSE)
# Sys.sleep(3)
