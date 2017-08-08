setwd("~/Dropbox/gates/sl3")
library(devtools)
library(formatR)
rebuild <- function() {
    tidy_dir(recursive = T, arrow = T)
    document()
    build()
    install()
    load_all()
}

rebuild()

