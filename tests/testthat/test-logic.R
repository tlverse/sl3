## working in progress

library(LogicReg)
library(data.table)
set.seed(1234)
n <- 1e4
p <- 100
# these two define the DGP (randomly)
p_X <- runif(p, 0.2, 0.8)
beta <- rnorm(p)

# simulate from the DGP
X <- sapply(p_X, function(p_Xi)rbinom(n, 1, p_Xi))
p_Yx <- plogis(X%*%beta)
Y <- rbinom(n, 1, p_Yx)
data <- data.table(X,Y)

fit = logreg(resp = Y, bin = X, type = 1, select = 1, penalty = 2)

predict(fit)
