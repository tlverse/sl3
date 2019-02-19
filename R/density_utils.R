make_bins <- function(x, type = c("equal_range","equal_mass"), n_bins = NULL){
  # clean up arguments
  type <- match.arg(type)
  
  # set grid along x
  if (type == "equal_range") {
    bins <- ggplot2::cut_interval(x, n_bins,
                                  right = FALSE,
                                  ordered_result = TRUE, dig.lab = 12
    )
  } else if (type == "equal_mass") {
    bins <- ggplot2::cut_number(x, n_bins,
                                right = FALSE,
                                ordered_result = TRUE, dig.lab = 12
    )
  }
  
  # https://stackoverflow.com/questions/36581075/extract-the-breakpoints-from-cut
  breaks_left <- as.numeric(sub(".(.+),.+", "\\1", levels(bins)))
  breaks_right <- as.numeric(sub(".+,(.+).", "\\1", levels(bins)))
  breaks <- c(breaks_left[1],breaks_right)
  return(breaks)
}

#' @importFrom ggplot2 cut_interval cut_number
discretize_variable <- function(x, type = c("equal_range","equal_mass"), n_bins = NULL, breaks = NULL){

  if(is.null(breaks)){
    breaks <- make_bins(x, type, n_bins)
  }
    
    # for predict method, only need to assign observations to existing intervals
    # NOTE: findInterval() and cut() might return slightly different results...
    bin_id <- findInterval(x, breaks, all.inside = TRUE)
    x_in_bin <- x-breaks[bin_id]
    list(x_discrete = bin_id, x_in_bin = x_in_bin, breaks = breaks)
}
