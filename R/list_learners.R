get_all_learners <- function(){
  # search for objects named like sl3 learners
  learner_names <- apropos("^Lrnr_")
  learners <- mget(learner_names, inherits=TRUE)
  
  # verify that learner inherits from Lrnr_base (and is therefore an actual sl3 Learner)
  is_learner_real <- sapply(learners, `[[`, "inherit") == "Lrnr_base"
  return(learners[which(is_learner_real)])
}

get_learner_class_properties <- function(learner_class){
  return(learner_class$private_fields$.properties)
}

#' @rdname list_learners
#' @export
sl3_list_properties <- function(){
  learners <- get_all_learners()
  properties <- lapply(learners, get_learner_class_properties)
  
  return(sort(unique(unlist(properties))))
}

#' List sl3 Learners
#' 
#' Lists learners in \code{sl3} (defined as objects that start with \code{Lrnr_} and inherit from \code{Lrnr_base})
#' @rdname list_learners
#' @param properties a vector of properties that learners must match to be returned
#' @return a vector of learner names that match the property list
#' @importFrom utils apropos
#' @export
sl3_list_learners <- function(properties=c()){
  learners <- get_all_learners()
  
  # filter by properties list
  learner_has_properties = function(learner_class, properties){
    return(all(properties %in% get_learner_class_properties(learner_class)))
  }
  
  match_properties <- sapply(learners, learner_has_properties, properties)
  matched_learners <- names(learners)[which(match_properties)]
  return(matched_learners)
}