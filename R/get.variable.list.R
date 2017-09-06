#' Returns a list of Climdex variables given constraints
#'
#' Returns a list of Climdex variables given constraints.
#'
#' This function takes a character vector which specifies what source data is present and a time resolution, and generates a list of names consisting of the variable and the time resolution, separated by an underscore.
#'
#' @param source.data.present A vector of strings naming the data that's present; at least one of (tmin, tmax, prec, tavg).
#' @param metadata.config config object read using \code{read_json_metadata_config_file}. This contains all the metadata such as the output long names of the indices in the output NCDF files.
#' @param time.resolution The time resolutions to compute indices at. See \code{\link{create.indices.from.files}}.
#' @param climdex.vars.subset A character vector of lower-case names of Climdex indices to calculate (eg: tr, fd, rx5day). See \code{\link{create.indices.from.files}}.
#' @return A character vector containing variable names with time resolutions appended.
#'
#' @seealso \code{\link{create.indices.from.files}}
#' @examples
#' ## Get all variables which require tmin and/or tmax, for all time resolutions.
#' var.list1 <- get.climdex.variable.list(c("tmax", "tmin"))
#'
#' ## Get all variables which require prec with an annual time resolution.
#' var.list2 <- get.climdex.variable.list("prec", time.resolution="annual")
#'
#' ## Get the intersection of a set list of vars and available data.
#' sub.vars <- c("su", "id", "tr", "fd", "gsl", "csdi", "wsdi", "r10mm")
#' var.list3 <- get.climdex.variable.list("tmax", climdex.vars.subset=sub.vars)
#'
#' @export
get.climdex.variable.list <- function(source.data.present, metadata.config, time.resolution=c("all", "annual", "monthly"), climdex.vars.subset=NULL) {
  time.res <- match.arg(time.resolution)
  vars.by.src.data.reqd = metadata.config$get.src.data.required()

  if(any(!(source.data.present %in% c("tmin", "tmax", "tavg", "prec"))))
    stop("Invalid variable listed in source.data.present.")

  if(all(c("tmax", "tmin") %in% source.data.present) && !("tavg" %in% source.data.present))
    source.data.present <- c(source.data.present, "tavg")

  climdex.vars <- unlist(vars.by.src.data.reqd[source.data.present])
  if(!is.null(climdex.vars.subset)) climdex.vars <- climdex.vars[climdex.vars %in% climdex.vars.subset]

  dat = metadata.config$get.variable.list(climdex.vars, time.resolution)
  return(dat)
}
