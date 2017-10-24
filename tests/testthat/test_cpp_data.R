context("test_cpp_data.R -- cpp data and subsets")

if (FALSE) {
    setwd("..")
    setwd("..")
    getwd()
    library("devtools")
    document()
    load_all("./")  # load all R files in /R and datasets in /data. Ignores NAMESPACE:
    setwd("..")
    install("sl3", build_vignettes = FALSE, dependencies = FALSE)  # INSTALL W/ devtools:
}

notrun.save.cpp.data <- function() {
  library("hbgd")
  data(cpp)
  cpp <- data.table(cpp)
  cpp <- cpp[, !names(cpp) %in% c("geniq", "sysbp", "diabp", "preeclmp"), with = FALSE]
  cpp_1yr <- cpp[agedays==366, ]
  # --------------------------------
  # save as compressed R file
  # --------------------------------
  require("tools")
  save(cpp, compress = TRUE, file = "./data/cpp.rda", compression_level = 9)
  resaveRdaFiles("./data/cpp.rda", compress = "bzip2")

  save(cpp_1yr, compress = TRUE, file = "./data/cpp_1yr.rda", compression_level = 9)
  resaveRdaFiles("./data/cpp_1yr.rda", compress = "bzip2")
}

library(testthat)
library(sl3)

data(cpp)
data(cpp_1yr)

cpp <- cpp[!is.na(cpp[, "haz"]), ]
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
cpp[is.na(cpp)] <- 0
outcome <- "haz"
cpp <- cpp[1:150, ]

