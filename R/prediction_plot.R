#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_line facet_grid scale_color_discrete xlab ylab
prediction_plot <- function(learner_fit){
  training_task <- learner_fit$training_task
  outcome_type <- training_task$outcome_type
  
  #todo: make these cv predictions
  predictions <- learner_fit$predict()
  observed <- training_task$Y
  
  if(outcome_type$type=="continuous"){
    pred_data <- data.table(pred=predictions, obs=observed)
    pred_plot <- ggplot(pred_data, aes(x=obs, y=pred))+geom_point()+geom_abline()+
      xlab("Observed")+ylab("Predicted")+theme_bw()+geom_smooth(se=FALSE)
  } else {
    unpacked <- unpack_predictions(predictions)
    unpacked <- as.data.table(unpacked)
    setnames(unpacked, outcome_type$levels)
    set(unpacked, , "observed", observed)
    long <- melt.data.table(unpacked, id=c("observed"), measure=outcome_type$levels, variable="category")
    cutoffs <- sort(unique(long$value)) #todo: do something smarter for big data
    long[ , accurate:=category==observed]
    all_auc_data <- lapply(cutoffs, function(cutoff){
      auc_data <- long[, list(positive_rate=mean(value>cutoff), cutoff=cutoff), by=list(observed, accurate)]
    })
    auc_data <- rbindlist(all_auc_data)
    wide <- dcast(auc_data, cutoff+observed~accurate, value.var="positive_rate")
    pred_plot <- ggplot(wide, aes(x=`FALSE`, y=`TRUE`, color=observed))+geom_line()+
      geom_segment(data=data.table(1), x=0,y=0,xend=1,yend=1, color="black")+
      theme_bw()+theme(legend.position="bottom")+
      xlab("False Positive Rate")+ylab("True Positive Rate")+scale_color_discrete("Observed")+coord_equal()
  }
  
  return(pred_plot)
}