context("test-density-sim.R -- Evaluate Density Learners on Simulated Density")


EY0 <- function(W) {
  W1 <- W[, 1]
  return(W1)
}

VY0 <- function(W){
  W2 <- W[,2]
  return(W2^2)
}

gen_data <- function(n = 1000, p = 4) {
  W <- matrix(rnorm(n * p), nrow = n)
  colnames(W)=sprintf("W%d",seq_len(p))
  EY <- sin(EY0(W)*(3*pi))
  VY <- rep(1,n)#VY0(W)
  Y <- rnorm(n, EY, sqrt(VY))
  fYW <- dnorm(Y, EY, sqrt(VY))
  
  data <- data.table(W,Y, EY, VY, fYW)
  
  return(data)
}

set.seed(1234)
data <- gen_data(1000)


Wnodes <- grep("^W", names(data), value = TRUE)
Ynode <- "Y"
task <- sl3_Task$new(data, covariates = Wnodes, outcome = Ynode)
lrnr_dd <- make_learner(Lrnr_density_discretize, make_learner(Lrnr_glmnet), "equal_mass",n_bins=20)
lrnr_dh <- make_learner(Lrnr_density_hse, make_learner(Lrnr_mean))
tt <- train_task(task,task$folds[[1]])
debugonce(lrnr_dd$base_train)
fit <- lrnr_dd$base_train(tt)
data$preds <- fit$predict(task)
ggplot(data,aes(x=Y,y=preds))+geom_point()+geom_line(aes(y=fYW),color="red")+theme_bw()

lrnr_cond <- Lrnr_condensier$new(nbins = 20, bin_method = "equal.mass", pool = TRUE)
lrnr_cond_up <- Lrnr_condensier$new(nbins = 20, bin_method = "equal.mass", pool = FALSE)
lrnr_ph <- make_learner(Lrnr_pooled_hazards)
lrnr_cond_rep <- make_learner(Lrnr_density_discretize, lrnr_ph, type="equal_mass", n_bins=20)
lrnr_dd_cat <- make_learner(Lrnr_density_discretize, make_learner(Lrnr_mean), type="equal_mass", n_bins=20)

lrnr_dd_cat_fit <- lrnr_dd_cat$train(task)
lrnr_cond_fit <- lrnr_cond$train(task)
lrnr_cond_up_fit <- lrnr_cond_up$train(task)
lrnr_cond_rep_fit <- lrnr_cond_rep$base_train(task)
microbenchmark( lrnr_dd_cat$train(task),lrnr_cond$train(task),lrnr_cond_up$train(task),lrnr_cond_rep$base_train(task))

data$cond_preds <- lrnr_cond_fit$predict(task)
data$cond_rep_preds <- lrnr_cond_rep_fit$predict(task)
data$dd_cat_preds <- lrnr_dd_cat_fit$predict(task)
data$cond_up_preds <- lrnr_cond_up_fit$predict(task)
mean(loss_loglik_true_cat(data$cond_preds, data$fYW)*data$fYW)
mean(loss_loglik_true_cat(data$cond_up_preds, data$fYW)*data$fYW)
mean(loss_loglik_true_cat(data$cond_rep_preds, data$fYW)*data$fYW)
mean(loss_loglik_true_cat(data$dd_cat_preds, data$fYW)*data$fYW)
ggplot(data,aes(x=Y,y=dd_cat_preds))+geom_point()+geom_line(aes(y=fYW),color="red")+theme_bw()
ggplot(data,aes(x=Y,y=cond_preds))+geom_point()+theme_bw()
## Define some learners:
lrn1 <- Lrnr_condensier$new(nbins = 5, bin_method = "equal.len", pool = FALSE)
lrn2 <- Lrnr_condensier$new(nbins = 5, bin_method = "equal.len", pool = TRUE)
lrn3 <- Lrnr_condensier$new(nbins = 7, bin_method = "equal.mass", pool = TRUE)
lrn4 <- Lrnr_condensier$new(
  nbins = 5, bin_method = "equal.len", pool = TRUE,
  condensier::speedglmR6$new()
  # bin_estimator = Lrnr_xgboost$new(nrounds = 20,
  # objective = 'reg:logistic')
)

sl <- Lrnr_sl$new(
  learners = list(lrn1, lrn2, lrn3, lrn4,lrnr_dd),
  metalearner = Lrnr_solnp_density$new()
)

#todo: fix names on stack failure
sl_fit <- sl$train(task)
sl_fit$cv_risk(loss_fun = loss_loglik_true_cat)
data$preds <- sl_fit$predict(task)
ggplot(data,aes(x=Y,y=preds))+geom_point()+geom_line(aes(y=fYW),color="red")+theme_bw()

xmat <- as.matrix(task$X)
basis_list <- hal9001::enumerate_basis(xmat)
col_lists <- lapply(basis_list,`[[`,"cols")
col_lists <- sapply(col_lists,paste,collapse=",")
basis_groups=unique(col_lists)

x_basis <- hal9001:::make_design_matrix(xmat, basis_list)
# copy_map <- hal9001:::make_copy_map(x_basis)
# unique_columns <- as.numeric(names(copy_map))
# x_basis <- x_basis[, unique_columns]
Y <- task$Y
Y_disc <- discretize_variable(Y,"equal_mass",20)
Yd <- factor(Y_disc$x_discrete)

Yb = as.numeric(Y>0)
pred = rep(mean(Y),n)
basis_group = basis_groups[[1]]

sub_basis = x_basis[,col_lists==basis_groups[2]]
library(glmnet)
system.time({
  mn_glmnet_fast <- cv.glmnet(x=sub_basis,y=Y, 
                              family="gaussian",intercept=TRUE, maxit=10,thresh=0.1, nfold=3)
  
})

lassi_object <- methods::new(hal9001:::Lassi, sub_basis, Y, nlambda=100, lambda_min_ratio=0.01, center=TRUE)
system.time({
  for(i in 1:100){
    lassi_object$lassi_fit_cd(i,FALSE,1)
  }
})

lassi_object
system.time({
  g <- speedglm.wfit(X=sub_basis,y=Y,maxit=1)
})

system.time({
  mn_glmnet_fast <- cv.glmnet(x=x_basis,y=Y, family="gaussian",intercept=TRUE)
})

library(glmnet)
#32s for 3000 basis
#39 for 1000 basis (wtf)
#54 for 1000 basis with no info (wtf,wtf!)
x_basis_red <- x_basis[,col_lists=="1"]
x_basis_red2 <- x_basis[,col_lists=="2"]

basis_groups = unique(basis_lists)
foldid <- rep(1:10,each=100)

foldid_fast <- ceiling(foldid/2)
Yb = as.numeric(Y>0)
system.time({
  mn_glmnet_full <- cv.glmnet(x=x_basis,y=Yb, family="binomial",foldid=foldid)
})

system.time({
  mn_glmnet_fast <- cv.glmnet(x=x_basis,y=Yb, family="binomial",maxit=1,thresh=1,foldid=foldid_fast,nlambda=10)
})

col_lists <- lapply(basis_list,`[[`,"cols")
col_lists <- sapply(col_lists,paste,collapse=",")
basis_groups=unique(col_lists)

system.time({
  screening <- sapply(basis_groups,function(basis_group){
    basis_subset=sample(which(col_lists==basis_group),100)
    sub <- x_basis[,basis_subset]
    screen_glmnet <- cv.glmnet(x=sub,y=Yb, family="binomial",maxit=1,thresh=1,foldid=foldid_fast,nlambda=10)
    reduction <- min(screen_glmnet$cvm)/screen_glmnet$cvm[1]
  })
})
coefs <- coef(mn_glmnet_fast)[-1]
group_norms <- sapply(basis_groups,function(basis_group){
  group_coefs <- coefs[col_lists==basis_group]
  sum(abs(group_coefs[group_coefs!=0]))
  })
avg_beta <- group_norms/n
lambda_min <- mn_glmnet_fast$lambda[which.min(mn_glmnet_fast$cvm)]
lambda_share <- lambda_min/length(group_norms)

keep_groups <- basis_groups[which(screening<0.95)]
x_basis_sub <- x_basis[,col_lists%in%keep_groups]

system.time({
  mn_glmnet_fast <- cv.glmnet(x=x_basis_sub,y=Yb, family="binomial",maxit=1,thresh=1,foldid=foldid_fast,nlambda=10)
})

lambda_0 <- mn_glmnet_fast$lambda[1]
lambda_min <- mn_glmnet_fast$lambda[which.min(mn_glmnet_fast$cvm)]
lambda_seq <- exp(seq(log(lambda_0),to=log(lambda_min),length=50))

system.time({
  mn_glmnet_full2 <- cv.glmnet(x=x_basis_sub,y=Yb, family="binomial",lambda=lambda_seq, foldid=foldid)
  
})


# stop running down lambda as cv increases
fullres <- data.table(lambda=mn_glmnet_full$lambda,cvm=mn_glmnet_full$cvm,variable="full")
fastres <- data.table(lambda=mn_glmnet_fast$lambda,cvm=mn_glmnet_fast$cvm,variable="fast")
fullres2 <- data.table(lambda=mn_glmnet_full2$lambda,cvm=mn_glmnet_full2$cvm,variable="full2")
long <- rbindlist(list(fullres,fastres,fullres2))
# z <- data.table(id=1:96,full=mn_glmnet_full$cvm[1:96],fast=mn_glmnet_fast$cvm[1:96],full2=c(mn_glmnet_full2$cvm,rep(NA,59)))
# long <- melt(z,id="id",measure=c("full","fast","full2"))
ggplot(long,aes(x=log(1/lambda),y=cvm,color=factor(variable)))+geom_point()+theme_bw()
zc <- as.vector(coef(mn_glmnet))
preds <- predict(mn_glmnet,newx=x_basis)
plot(EY,preds)
