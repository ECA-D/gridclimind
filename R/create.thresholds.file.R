#' Creates Climdex thresholds output file.
#'
#' Creates Climdex thresholds output file.
#'
#' This function creates a file suitable for outputting thresholds to, with all variables that can be created with the input data present in the file.
#'
#' @param thresholds.file The filename to be used for the thresholds file.
#' @param f The file(s) being used as sources for metadata.
#' @param ts The associated time data, as created by \code{nc.get.time.series}.
#' @param v.f.idx A mapping from variables to files, as created by \code{\link{get.var.file.idx}}.
#' @param variable.name.map A mapping from standardized names (tmax, tmin, prec) to NetCDF variable names.
#' @param base.range The base range; a vector of two numeric years.
#' @param dim.size Dimension sizes for the input.
#' @param dim.axes Dimension axes for the input.
#' @param threshold.dat Threshold metadata, as provided by \code{\link{get.thresholds.metadata}}.
#' @param author.data A vector containing named elements describing the author; see \code{\link{create.indices.from.files}}.
#' @return An object of class \code{ncdf4}.
#'
#' @examples
#' \donttest{
#' ## Establish basic inputs.
#' author.data <- list(institution="Looney Bin", institution_id="LBC")
#' input.files <- c("pr_NAM44_CanRCM4_ERAINT_r1i1p1_1989-2009.nc")
#'
#' ## Prepare derived inputs.
#' f <- lapply(input.files, ncdf4::nc_open)
#' variable.name.map <- c(tmax="tasmax", tmin="tasmin", prec="pr")
#' f.meta <- create.file.metadata(f, variable.name.map)
#' threshold.dat <- get.thresholds.metadata(names(f.meta$v.f.idx))
#'
#' ## Create output file
#' thresh.file <- create.thresholds.file("thresh.nc", f, f.meta$ts, f.meta$v.f.idx, variable.name.map,
#'                                       c(1981,1990), f.meta$dim.size, f.meta$dim.axes,
#'                                       threshold.dat, author.data)
#' }
#'
#' @export
create.thresholds.file <- function(thresholds.file, f, ts, v.f.idx, variable.name.map, base.range, dim.size, dim.axes, threshold.dat, author.data) {
  exemplar.file <- f[[v.f.idx[1]]]
  exemplar.var.name <- variable.name.map[names(v.f.idx)[1]]
  exemplar.var <- exemplar.file$var[[exemplar.var.name]]
  num.thresholds <- ifelse(is.null(attr(ts, "dpy")), 365, attr(ts, "dpy"))
  cal <- attr(ts, "cal")

  ## Get time metadata...
  old.time.dim <- exemplar.var$dim[[which(dim.axes == "T")]]
  time.units <- old.time.dim$units
  time.units.split <- strsplit(time.units, " ")[[1]]
  time.origin <- if(time.units.split[2] == "as") format(trunc(min(ts), units="days"), "%Y-%m-%d") else time.units.split[3]
  time.dim.name <- old.time.dim$name
  old.time.bnds.att <- ncdf4::ncatt_get(exemplar.file, time.dim.name, "bounds")
  time.bnds.name <- if(old.time.bnds.att$hasatt) old.time.bnds.att$value else "time_bnds"

  ## Set up time variables
  out.time <- as.numeric(julian(as.PCICt(paste(floor(mean(base.range)), 1:num.thresholds, sep="-"), attr(ts, "cal"), format="%Y-%j"), as.PCICt(time.origin, cal)), units="days")
  out.time.dim <- ncdf4::ncdim_def("time", paste("days since", time.origin), out.time, unlim=TRUE, calendar=cal, longname="time")

  ## Set up bounds
  input.bounds <- ncdf4.helpers::nc.get.dim.bounds.var.list(exemplar.file)
  input.bounds <- input.bounds[input.bounds != time.bnds.name]
  input.dim.names <- ncdf4.helpers::nc.get.dim.names(exemplar.file, exemplar.var.name)
  input.varname.list <- c(input.bounds, input.dim.names)

  bnds.dim <- ncdf4::ncdim_def("bnds", "", 1:2, create_dimvar=FALSE)
  if(length(input.bounds) > 0)
    bnds.dim <- exemplar.file$var[[input.bounds[1]]]$dim[[1]]
  out.time.bnds <- as.numeric(julian(as.PCICt(c(paste(base.range[1], 1:num.thresholds, sep="-"), paste(base.range[2], 1:num.thresholds, sep="-")), attr(ts, "cal"), format="%Y-%j"), as.PCICt(time.origin, cal)), units="days")
  dim(out.time.bnds) <- c(num.thresholds, 2)
  out.time.bnds <- t(out.time.bnds)
  out.time.bnds.var <- ncdf4::ncvar_def(time.bnds.name, '', list(bnds.dim, out.time.dim), longname='', prec="double")

  input.bounds.vars <- c(lapply(input.bounds, function(x) { exemplar.file$var[[x]] }), list(out.time.bnds.var))
  input.bounds.data <- c(lapply(input.bounds, function(x) { ncdf4::ncvar_get(exemplar.file, x) }), list(out.time.bnds))
  all.bounds <- c(input.bounds, time.bnds.name)
  names(input.bounds.data) <- names(input.bounds.vars) <- all.bounds

  ## Set up 2d and 3d dims
  out.dims.3d <- list(exemplar.var$dim[[which(dim.axes == 'X')]], exemplar.var$dim[[which(dim.axes == 'Y')]], out.time.dim)
  out.dims.2d <- list(exemplar.var$dim[[which(dim.axes == 'X')]], exemplar.var$dim[[which(dim.axes == 'Y')]])
  out.vars <- sapply(names(threshold.dat), function(n) {
    tinfo <- threshold.dat[[n]]
    if(tinfo$has.time)
      ncdf4::ncvar_def(n, tinfo$units, out.dims.3d, 1e20, tinfo$longname, prec="double")
    else
      ncdf4::ncvar_def(n, tinfo$units, out.dims.2d, 1e20, tinfo$longname, prec="double")
  }, simplify=FALSE)

  ## Tack bounds vars onto var list so they get created...
  all.vars <- c(input.bounds.vars, out.vars)

  ## Create file
  thresholds.netcdf <- ncdf4::nc_create(thresholds.file, all.vars, force_v4=TRUE)
  out.dim.axes <- c("X", "Y", "T")

  ## Copy attributes for all variables plus global attributes
  ncdf4::nc_redef(thresholds.netcdf)
  ncdf4.helpers::nc.copy.atts(exemplar.file, 0, thresholds.netcdf, 0, definemode=TRUE)
  for(v in input.varname.list) {
    ncdf4.helpers::nc.copy.atts(exemplar.file, v, thresholds.netcdf, v, definemode=TRUE)
  }

  put.ETCCDI.atts(thresholds.netcdf, "monClim", ncdf4::ncatt_get(exemplar.file, 0, "title")$value, author.data, definemode=TRUE)

  ## Attach history data to threshold data.
  lapply(out.vars, function(v) {
    put.history.att(thresholds.netcdf, v, definemode=TRUE)
    ncdf4::ncatt_put(thresholds.netcdf, v, "base_period", paste(base.range, collapse="-"), definemode=TRUE)
  })
  ncdf4::nc_enddef(thresholds.netcdf)

  ## Put bounds data.
  for(v in all.bounds) {
    ncdf4::ncvar_put(thresholds.netcdf, v, input.bounds.data[[v]])
  }

  return(thresholds.netcdf)
}
