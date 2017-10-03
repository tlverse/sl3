library(devtools)
load_all()
library(SuperLearner)
library(data.table)
# 2
# adapted from library(SIS)
set.seed(1)
# training
b <- c(2, 2, 2, -3*sqrt(2))
n <- 150
p <- 200
truerho <- 0.5
corrmat <- diag(rep(1-truerho, p)) + matrix(truerho, p, p)
corrmat[, 4] = sqrt(truerho)
corrmat[4, ] = sqrt(truerho)
corrmat[4, 4] = 1
cholmat <- chol(corrmat)
x <- matrix(rnorm(n*p, mean=0, sd=1), n, p)
x <- x 
feta <- x[, 1:4] 
fprob <- exp(feta) / (1 + exp(feta))
y <- rbinom(n, 1, fprob)

# test
m <- 10000
newx <- matrix(rnorm(m*p, mean=0, sd=1), m, p)
newx <- newx 
newfeta <- newx[, 1:4] 
newfprob <- exp(newfeta) / (1 + exp(newfeta))
newy <- rbinom(m, 1, newfprob)

DATA2 <- data.frame(Y = y, X = x)
Y <- y
X <- x
id <- NULL
obsWeights <- NULL
newDATA2 <- data.frame(Y = newy, X=newx)

create.SL.knn <- function(k = c(20, 30)) {
  for(mm in seq(length(k))){
    eval(parse(text = paste('SL.knn.', k[mm], '<- function(..., k = ', k[mm],
                            ') SL.knn(..., k = k)', sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.knn(c(20, 30, 40, 50, 60, 70))

# library with screening
SL.library <- list(c("SL.glmnet", "All"), c("SL.glm", "screen.randomForest"),
                   "SL.randomForest", "SL.svm",
                   c("SL.earth", "screen.randomForest"))
system.time({
test <- SuperLearner(Y = DATA2$Y, X = DATA2[, -1], newX = newDATA2[, -1],
                     SL.library = SL.library, verbose = TRUE, family = binomial())
})
# test
plan(sequential)
system.time({
  test <- SuperLearner3(Y = DATA2$Y, X = DATA2[, -1], newX = newDATA2[, -1],
                       SL.library = SL.library, verbose = TRUE, family = binomial())
})
# test


#define task from arguments
task_data <- as.data.table(X)
covariates <- copy(names(task_data))
#todo: check if these columns already exist in X
set(task_data, j="Y", value = Y)

if(!is.null(obsWeights)){
  set(task_data, j="weights", value = obsWeights)
  weightvar <- "weights"
} else {
  weightvar <- NULL
}

if(!is.null(id)){
  set(task_data, j="id", value = id)
  idvar <- "id"
} else {
  idvar <- NULL
}

training_task <- sl3_Task$new(data=task_data, covariates <- covariates, outcome = "Y", id=idvar, weights=weightvar)

learners <- c("SL.glm", "SL.glmnet","SL.randomForest","SL.svm", "SL.mean")

folds <- training_task$folds

fold_vec <- lapply(folds,function(fold)validation())
set.seed(1234)
test <- SuperLearner(Y = DATA2$Y, X = DATA2[, -1], newX = newDATA2[, -1],
                     SL.library = learners, verbose = TRUE, family = binomial(), cvControl = list(validRows = fold_vec))

test
learner_objs <- lapply(learners,Lrnr_pkg_SuperLearner$new, family="binomial")
metalearner <- Lrnr_pkg_SuperLearner_method$new(method)
sl <- Lrnr_sl$new(learner_objs,metalearner,folds)
plan(sequential)
set.seed(1234)
sl_fit <- sl$train(training_task)

sl3z <- as.matrix(sl_fit$fit_object$cv_meta_task$X)
slz <- test$Z
plot(sl3z[,1],slz[,1])
diag(cor(slz,sl3z))
sl_fit
test
stack <- do.call(Stack$new,learner_objs)
cv_stack <- Lrnr_cv$new(stack, folds)
csf <- cv_stack$train(training_task)
z3 <- as.matrix(csf$predict())
head(z3)
head(Y)
