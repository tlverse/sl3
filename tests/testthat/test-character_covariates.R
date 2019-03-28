context("test-character_covariates.R -- Automatically cast character covariates to factors")

if (FALSE) {
  setwd("..")
  setwd("..")
  getwd()
  library("devtools")
  document()
  load_all("./") # load all R files in /R and datasets in /data. Ignores NAMESPACE:
  setwd("..")
  install("sl3", build_vignettes = FALSE, dependencies = FALSE) # INSTALL W/ devtools:
}

data(cpp_imputed)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
outcome <- "haz"
cpp_imputed$sexn <- as.character(cpp_imputed$sexn)

expect_warning(
  task_character_to_factor <- make_sl3_Task(cpp_imputed, covars, outcome),
  "Character covariates found: sexn;\nConverting these to factors"
)

expect_equal(class(task_character_to_factor$get_node("covariates")$sexn), "factor")
