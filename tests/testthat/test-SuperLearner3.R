context("test-SuperLearner3.R -- Replicate legacy SuperLearner interface")

library(devtools)
library(SuperLearner)
library(data.table)
# 2
# adapted from library(SIS)
set.seed(1)
# training
b <- c(2, 2, 2, -3 * sqrt(2))
n <- 150
p <- 10
truerho <- 0.5
corrmat <- diag(rep(1 - truerho, p)) + matrix(truerho, p, p)
corrmat[, 4] <- sqrt(truerho)
corrmat[4, ] <- sqrt(truerho)
corrmat[4, 4] <- 1
cholmat <- chol(corrmat)
x <- matrix(rnorm(n * p, mean = 0, sd = 1), n, p)
x <- x
feta <- x[, 1:4]
fprob <- exp(feta) / (1 + exp(feta))
y <- rbinom(n, 1, fprob)

# test
m <- 10000
newx <- matrix(rnorm(m * p, mean = 0, sd = 1), m, p)
newx <- newx
newfeta <- newx[, 1:4]
newfprob <- exp(newfeta) / (1 + exp(newfeta))
newy <- rbinom(m, 1, newfprob)

DATA2 <- data.frame(Y = y, X = x)
Y <- y
X <- x
id <- NULL
obsWeights <- NULL
newDATA2 <- data.frame(Y = newy, X = newx)
method <- "method.NNLS2"

# library with screening
SL.library <- list(
  c("SL.glmnet", "All"), c("SL.glm", "screen.randomForest"),
  "SL.randomForest", "SL.svm",
  c("SL.earth", "screen.randomForest")
)
system.time({
  test_SuperLearner <- SuperLearner(
    Y = DATA2$Y, X = DATA2[, -1], newX = newDATA2[, -1],
    SL.library = SL.library, verbose = TRUE, family = binomial()
  )
})

# test
system.time({
  test_sl3 <- SuperLearner3(
    Y = DATA2$Y, X = DATA2[, -1], newX = newDATA2[, -1],
    SL.library = SL.library, verbose = TRUE, family = binomial()
  )
})
