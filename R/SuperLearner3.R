#generate learners from SL.library
convert_SL.library <- function(SL.library, family = "gaussian"){
  screeners <- sapply(SL.library,function(learner){ifelse(length(learner)==1,"All",learner[[2]])})
  learners <- sapply(SL.library,`[`,1) 
  
  unique_screeners <- unique(screeners)
  
  #generate one stack per screener in a pipeline with that screener  
  learner_objs <- lapply(unique_screeners,function(screener){
    learners_with_screener <- learners[screener==screeners]
    learner_objs <- lapply(learners_with_screener, function(learner) make_learner(Lrnr_pkg_SuperLearner, SL_wrapper = learner, family = family))
    learner_stack <- make_learner(Stack,learner_objs)
    
    
    if(screener != "All"){
      learner_screener <- make_learner(Lrnr_pkg_SuperLearner_screener, SL_wrapper = screener, family = family)
      learner_pipe <- Pipeline$new(learner_screener, learner_stack)
      return(learner_pipe)
    } else {
      return(learner_stack)      
    }
  })
}



SuperLearner3 <- function (Y, X, newX = NULL, family = gaussian(), SL.library, 
                           method = "method.NNLS", id = NULL, verbose = FALSE, control = list(), 
                           cvControl = list(), obsWeights = NULL, env = parent.frame()){
  
  
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
  
  training_task <- sl3_Task$new(data=task_data, covariates <- covariates, outcome = "Y", id = idvar, weights = weightvar)
  learner_objs <- convert_SL.library(SL.library, family = family)

  method_obj <- do.call(method, list())
  metalearner <- Lrnr_pkg_SuperLearner_method$new(method_obj)
  
  sl_learner <- Lrnr_sl$new(learner_objs,metalearner, folds=training_task$folds)
  sl_fit <- sl_learner$train(training_task)
  # preds <- sl_fit$predict()
}