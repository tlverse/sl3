#' @importFrom utils packageVersion
NULL

#' Querying/setting a single \code{sl3} option
#'
#' To list all \code{sl3} options, just run this function without any parameters
#'  provided. To query only one value, pass the first parameter. To set that,
#'  use the \code{value} parameter too.
#'
#' @param o Option name (string).
#' @param value Value to assign (optional)
#' @export
#' @examples \dontrun{
#' sl3Options()
#' sl3Options('sl3.verbose')
#' sl3Options('sl3.temp.dir')
#' sl3Options('sl3.verbose', TRUE)
#' }
#
sl3Options <- function (o, value)  {
  res <- options()[grep("sl3", names(options()))]
  if (missing(value)) {
    if (missing(o))
        return(res)
    if (o %in% names(res))
        return(res[[o]])
    print("Possible `sl3` options:")
    print(names(res))
    stop(paste0(o, ": this options does not exist"))
  } else {
    if (!o %in% names(res))
      stop(paste("Invalid option name:", o))
    if (is.null(value)) {
      res[o] <- list(NULL)
    }
    else {
      res[[o]] <- value
    }
    options(res[o])
  }
}

.onLoad <- function(libname, pkgname) {
  # Runs when loaded but not attached to search() path; e.g., when a package just Imports (not Depends on) sl3
  # Set options for the speed boost in v1.8.0 by avoiding 'default' arg of getOption(,default=)
  opts = list("sl3.verbose"    = FALSE,
              "sl3.file.path"  = tempdir(),
              "sl3.temp.dir"   = tempdir(),
              # sl3.file.name  = 'sl3-report-%T-%N-%n'
              "sl3.file.name"  = paste0('sl3-report-', Sys.Date()),
              "sl3.memoise.learner" = FALSE,
              "sl3.save.training" = TRUE
            )
  # for (i in setdiff(names(opts),names(options()))) {
  #   browser()
  #   eval(parse(text=paste("options(",i,"=",as.character(opts[i]),")",sep="")))
  # }
  toset <- !(names(opts) %in% names(options()))
  if (any(toset)) options(opts[toset])
  invisible()
}

.onAttach <- function(libname, pkgname) {
  # Runs when attached to search() path such as by library() or require()
  if (interactive()) {
    v = packageVersion("sl3")
    d = read.dcf(system.file("DESCRIPTION", package="sl3"), fields = c("Packaged", "Built"))
    if(is.na(d[1])){
      if(is.na(d[2])){
        return() #neither field exists
      } else{
        d = unlist(strsplit(d[2], split="; "))[3]
      }
    } else {
      d = d[1]
    }
    # dev = as.integer(v[1,3])%%2 == 1  # version number odd => dev
    packageStartupMessage("sl3 ", v)
    # if (dev && (Sys.Date() - as.Date(d))>28)
    #     packageStartupMessage("**********\nThis development version of sl3 was built more than 4 weeks ago. Please update.\n**********")
    packageStartupMessage('Please note the package is in early stages of development. Check often for updates and report bugs at http://github.com/jeremyrcoyle/sl3.', '\n')
  }
}

