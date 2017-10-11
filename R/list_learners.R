sl3_Learner_Registry<- R6Class(classname = "sl3_learner_registry",
                    portable = TRUE,
                    class = TRUE,
                    public = list(
                      set_learners = function(learners){
                        private$.learners = learners
                      },
                      register_learner = function(learner_class){
                        private$.learners <- c(private$.learners, learner_class)
                      },
                      learner_has_properties = function(learner_class, properties){
                        return(all(properties %in% learner_class$private_fields$.properties))
                      },
                      find_learners = function(properties = list()){
                        all_learners <- private$.learners
                        match_properties <- sapply(all_learners, self$learner_has_properties, properties)
                        matched_learners <- names(all_learners)[which(match_properties)]
                        return(matched_learners)
                      }
                    ),
                    active = list(),
                    private = list(
                      .learners = list()
                    ))
                    

sl3_learner_registry <- sl3_Learner_Registry$new()

sl3_list_learners <- function(properties=list()){
  sl3_learner_registry$find_learners(properties)
}