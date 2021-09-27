# hierarchical sl test


rm(list = ls())
library(devtools)
library(origami)
library(data.table)
# load sl3 test package, path = .../sl3
test_path <- paste0(here::here())
load_all(test_path)

# load data
washb_data <- fread(
  paste0(
    "https://raw.githubusercontent.com/tlverse/tlverse-data/master/",
    "wash-benefits/washb_data.csv"
  ),
  stringsAsFactors = TRUE
)

# randomly assign to clusters
washb_data$id <- rep(c(1:313), 15)

# select a small set of covar for test
covar <- c("month", "aged", "sex", "momedu", "hfiacat")


###################### test Lrnr_sl with Lrnr_aggregate ############################

lrnr_glm <- make_learner(Lrnr_glm)
lrnr_lasso <- make_learner(Lrnr_glmnet)
lrnr_aggregate <- make_learner(Lrnr_aggregate)
lrnr_clust <- make_learner(Pipeline, lrnr_aggregate, lrnr_glm)
lrnr_indiv <- make_learner(Pipeline, lrnr_glm, lrnr_aggregate)

sl3_task <- make_sl3_Task(
  data = washb_data,
  covariates = c("tr",covar),
  outcome = 'whz',
  outcome_type = 'continuous',
  id = 'id'
)

hierar_lib <- make_learner(Stack,
                           lrnr_indiv,
                           lrnr_clust)

discrete_sl_metalrn <- Lrnr_cv_selector$new()

hierar_sl <- make_learner(Lrnr_sl,
                          learners = hierar_lib,
                          outcome_type = 'continuous',
                          metalearner = discrete_sl_metalrn)


hierar_sl_fit <- hierar_sl$train(sl3_task)
hierar_sl_fit$predict_fold(task = sl3_task, fold_number = "full")
hierar_sl_fit$predict_fold(task = sl3_task, fold_number = "validation")

# TBD coef does not display for discrete sl
hierar_sl_fit$cv_risk(loss_squared_error)

# debug_train(hierar_sl)
# debug_predict(hierar_sl_fit)


# ###################### test Pipeline with Lrnr_aggregate ############################
# # define sl3 task
# sl3_task <- make_sl3_Task(
#   data = washb_data,
#   covariates = c("tr",covar),
#   outcome = 'whz',
#   outcome_type = 'continuous',
#   id = 'id'
# )
# 
# # task <- sl3_task
# 
# # test pre
# lrnr_glm <- make_learner(Lrnr_glm)
# lrnr_aggregate <- make_learner(Lrnr_aggregate)
# lrnr_clust <- make_learner(Pipeline, lrnr_aggregate, lrnr_glm)
# # la_fit <- lrnr_aggregate$train(task)
# # la_fit$chain(task)$data
# # debug_predict(la_fit)
# # la_fit$predict(task)
# 
# glm_fit <- lrnr_glm$train(sl3_task)
# pre_aggre_fit <- lrnr_aggregate$train(sl3_task)
# clust_fit <- lrnr_clust$train(sl3_task)
# 
# 
# # test post
# lrnr_indiv <- make_learner(Pipeline, lrnr_glm, lrnr_aggregate)
# indiv_fit <- lrnr_indiv$train(sl3_task)
# 
# # check whether the pipe learner has same preds as
# # manually aggregating glm results
# 
# # temp <- glm_fit$chain(sl3_task)$data
# # temp_aggre <- temp[,lapply(.SD, mean),by=list(id)]
# # temp_aggre <- temp_aggre$predictions
# #
# # all(temp_aggre == indiv_fit$predict())
# 
# 
# # test t v
# washb_data_t <- washb_data[!id %in% c(1:50), ]
# washb_data_v <- washb_data[id %in% c(1:50), ]
# 
# sl3_task_t <- make_sl3_Task(
#   data = washb_data_t,
#   covariates = c("tr",covar),
#   outcome = 'whz',
#   outcome_type = 'continuous',
#   id = 'id'
# )
# 
# 
# sl3_task_v <- make_sl3_Task(
#   data = washb_data_v,
#   covariates = c("tr",covar),
#   outcome = 'whz',
#   outcome_type = 'continuous',
#   id = 'id'
# )
# 
# lrnr_aggregate <- make_learner(Lrnr_aggregate)
# lrnr_clust <- make_learner(Pipeline, lrnr_aggregate, lrnr_glm)
# lrnr_indiv <- make_learner(Pipeline, lrnr_glm, lrnr_aggregate)
# 
# clust_fit_t <- lrnr_clust$train(sl3_task_t)
# clust_fit_t$predict(sl3_task_v)
# 
# indiv_fit_t <- lrnr_indiv$train(sl3_task_t)
# indiv_fit_t$predict(sl3_task_v)
# 
# 
# 
# ###################### test Lrnr_cv with Lrnr_aggregate ############################
# 
# lrnr_glm <- make_learner(Lrnr_glm)
# lrnr_lasso <- make_learner(Lrnr_glmnet)
# lrnr_aggregate <- make_learner(Lrnr_aggregate)
# lrnr_clust <- make_learner(Pipeline, lrnr_aggregate, lrnr_glm)
# lrnr_indiv <- make_learner(Pipeline, lrnr_glm, lrnr_aggregate)
# 
# sl3_task <- make_sl3_Task(
#   data = washb_data,
#   covariates = c("tr",covar),
#   outcome = 'whz',
#   outcome_type = 'continuous',
#   id = 'id'
# )
# 
# hierar_lib <- make_learner(Stack,
#                            lrnr_indiv,
#                            lrnr_clust)
# 
# cv_stack <- Lrnr_cv$new(hierar_lib)
# hierar_cv_fit <- cv_stack$train(sl3_task)
# temp_pred <- hierar_cv_fit$predict()
# temp_mse <- hierar_cv_fit$cv_risk(loss_squared_error)
# 
# # debug_predict(hierar_cv_fit)
# # undebug_learner(hierar_cv_fit)
# 
# 
# hierar_lib <- make_learner(Stack,
#                            lrnr_indiv,
#                            lrnr_clust)
# 
# hierar_stack_fit <- hierar_lib$train(sl3_task)
# # temp_pred <- hierar_stack_fit$predict(sl3_task)
# # temp <- hierar_stack_fit$base_chain(sl3_task)
# 
# 
# ###################### test cross_validate (origami) with Lrnr_aggregate ############################
# # function to calculate cross-validated squared error
# cv_hierarchi <- function(fold, 
#                          data, 
#                          outcome = 'whz',
#                          id = 'id') {
#   
#   # get training and validation data
#   train_data <- training(data)
#   valid_data <- validation(data)
#   
#   # aggregate outcome 
#   main_cols <- c(outcome, id)
#   valid_main <- valid_data[,..main_cols]
#   agg_valid_main <- valid_main[,lapply(.SD, mean),by=list(id)]
#   valid_Y <- agg_valid_main[,..outcome][[1]]
#   # print(length(valid_Y))
#   
#   # get covs names
#   covs <- names(data)[which(!names(data) %in% c(outcome, id))]
#   
#   # define learners
#   lrnr_glm <- make_learner(Lrnr_glm)
#   lrnr_aggregate <- make_learner(Lrnr_aggregate)
#   
#   # cluster model
#   lrnr_clust <- make_learner(Pipeline, lrnr_aggregate, lrnr_glm)
#   
#   task_t <- make_sl3_Task(
#     data = train_data,
#     covariates = covs,
#     outcome = outcome,
#     id = id
#   )
#   clust_fit_t <- lrnr_clust$train(task_t)
#   
#   task_v <- make_sl3_Task(
#     data = valid_data,
#     covariates = covs,
#     outcome = outcome,
#     id = id
#   )
#   clust_pred <- clust_fit_t$predict(task_v)
#   clust_MSE <- mean((clust_pred - valid_Y)^2)
#   print(length(clust_pred) == length(valid_Y))
#   
#   
#   # individual model
#   lrnr_indiv <- make_learner(Pipeline, lrnr_glm, lrnr_aggregate)
#   
#   task_t <- make_sl3_Task(
#     data = train_data,
#     covariates = covs,
#     outcome = outcome,
#     id = id
#   )
#   indiv_fit_t <- lrnr_indiv$train(task_t)
#   
#   task_v <- make_sl3_Task(
#     data = valid_data,
#     covariates = covs,
#     outcome = outcome,
#     id = id
#   )
#   indiv_pred <- indiv_fit_t$predict(task_v)
#   indiv_MSE <- mean((indiv_pred - valid_Y)^2)
#   
#   out <- list(mse = data.frame(
#               fold = fold_index(),
#               clust_MSE = clust_MSE, 
#               indiv_MSE = indiv_MSE
#             ))
#   
#   return(out)
# }
# 
# 
# # run CV
# all_cols <- c(covar, 'tr', 'whz', 'id')
# washb_data_sub <- washb_data[,..all_cols]
# 
# mses <- cross_validate(
#   cv_fun = cv_hierarchi, 
#   folds = make_folds(washb_data_sub, cluster_ids = washb_data_sub$id), 
#   data = washb_data_sub,
#   use_future = FALSE
# )
# 
# mses
# 
# colMeans(mses$mse[, c("clust_MSE", "indiv_MSE")])











