context("Incorporating corrections for missingness in covariates")

library(data.table)
library(assertthat)
library(uuid)
library(sl3)
library(tmle3)
set.seed(34831)

# setup data for test
data(cpp)
data <- as.data.table(cpp)
data[, parity01 := as.numeric(data$parity > 0)]
data[, parity01_fac := factor(data$parity01)]
data[, haz01 := as.numeric(data$haz > 0)]

node_list <- list(
  W = c(
    "apgar1", "apgar5", "gagebrth", "mage",
    "meducyrs", "sexn"
  ),
  A = "waz",
  Y = "haz01"
)

# drop missing A for now, might add back to test later
missing_W <- apply(is.na(data[, c(node_list$W, node_list$A),
  with = FALSE
]), 1, any)
data <- data[!missing_W]

task <- sl3_Task$new(data, outcome=node_list$Y, covariates = node_list$W)
