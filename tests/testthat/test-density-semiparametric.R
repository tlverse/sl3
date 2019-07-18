context("test-density-semiparametric.R -- Lrnr_density_semiparametric")
set.seed(1234)
# define test dataset
<<<<<<< HEAD
n <- 1e6
x <- runif(n,0,3)
epsilon_x <- rnorm(n,0,0.5+sqrt(x))
# epsilon_x <- rnorm(n)
y <- 3*x + epsilon_x
=======
n <- 1e3
x <- rnorm(n)
epsilon <- rnorm(n)
y <- 3 * x + epsilon

data <- data.table(x = x, y = y)
>>>>>>> 36915f896dae9f9a38401eb522c67d11cb88c147

data <- data.table(x=x, x2=x^2, y=y)
covariates <- c("x")
task <- make_sl3_Task(data, covariates = covariates, outcome = "y")


# train
<<<<<<< HEAD
hse_learner <- make_learner(Lrnr_density_semiparametric, 
                            mean_learner=make_learner(Lrnr_glm))


mvd_learner <- make_learner(Lrnr_density_semiparametric, 
                            mean_learner=make_learner(Lrnr_glm),  
                            var_learner=make_learner(Lrnr_glm))

=======
hse_learner <- make_learner(Lrnr_density_hse, mean_learner = make_learner(Lrnr_glm_fast))
>>>>>>> 36915f896dae9f9a38401eb522c67d11cb88c147
hse_fit <- hse_learner$train(task)
mvd_fit <- mvd_learner$train(task)

<<<<<<< HEAD
x_grid <- seq(from=min(data$x),to=max(data$x),length=100)
y_grid <- seq(from=min(data$y),to=1.5*max(data$y),length=100)
pred_data <- as.data.table(expand.grid(x=x_grid, y=y_grid))
pred_data$x2 <- pred_data$x^2
pred_task <- make_sl3_Task(pred_data, covariates = covariates, outcome = "y")

pred_data$hse_preds <- hse_fit$predict(pred_task)
pred_data$mvd_preds <- mvd_fit$predict(pred_task)
pred_data[ , true_dens:= dnorm(x=y, mean=3*x, sd=abs(x))]

nll <- function(observed, pred){
  res <- -1 * observed * log(pred)
  res[observed<.Machine$double.eps] <- 0
  
  return(res)
}


hse_nll <- sum(nll(pred_data$true_dens, pred_data$hse_preds))
mvd_nll <- sum(nll(pred_data$true_dens, pred_data$mvd_preds))
print(hse_nll)
print(mvd_nll)
expect_lt(hse_nll,n)
expect_lt(mvd_nll,hse_nll)
long <- melt(pred_data,id=c("x","y","true_dens"),measure=c("hse_preds","mvd_preds","true_dens"))
x_samp <- sample(x_grid,20)
ggplot(long[x%in%x_samp],aes(x=y,y=value,color=variable))+geom_line()+facet_wrap(~round(x,5),scales="free_x")+theme_bw()+coord_flip()
=======
x_grid <- seq(from = min(data$x), to = max(data$x), length = 100)
y_grid <- seq(from = min(data$y), to = max(data$y), length = 100)
pred_data <- as.data.table(expand.grid(x = x_grid, y = y_grid))
pred_task <- make_sl3_Task(pred_data, covariates = c("x"), outcome = "y")

pred_data$dens_preds <- hse_fit$predict(pred_task)
pred_data[, true_dens := dnorm(x = y, mean = 3 * x)]
nll <- sum(-1 * pred_data$true_dens * log(pred_data$dens_preds))
expect_lt(nll, n)
>>>>>>> 36915f896dae9f9a38401eb522c67d11cb88c147
