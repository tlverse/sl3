#replace object with its uuid if available
replace_uuid <- function(object){
  uuid <- object$uuid
  if(is.null(uuid)){
    return(object)
  } else {
    return(uuid)
  }
}

#use uuid as serialised object for objects that have one
#' @importFrom digest digest
digest_uuid <- function(object){
  object_uuid <- lapply(object, replace_uuid)
  #todo: pull algo from existing cache
  return(digest(object_uuid, algo="xxhash64"))
}

#replace cache with the digest function with digest_uuid
cache_uuid <- function(cache){
  cache$digest <- digest_uuid
  
  return(cache)
}

