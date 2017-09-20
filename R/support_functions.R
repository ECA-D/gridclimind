unsquash.dims <- function(dat.dim, subset, f, n) {
  dim.axes <- ncdf4.helpers::nc.get.dim.axes(f, n)
  return(sapply(dim.axes, function(x) { if(any(names(subset) == x)) length(subset[[x]]) else f$dim[[names(dim.axes)[dim.axes == x]]]$len }))
}

get.time.origin <- function(f, dim.axes) {
  time.units <- f[[1]]$dim[[names(dim.axes)[which(dim.axes == "T")]]]$units
  time.units.split <- strsplit(gsub("[ ]+", " ", time.units), " ")[[1]]
  time.origin <- if(time.units.split[2] == "as") format(trunc(min(ts), units="days"), "%Y-%m-%d") else time.units.split[3]
  return(time.origin)
}

set.up.cluster <- function(parallel, type="SOCK") {
  ## Fire up the cluster...
  cluster <- NULL

  if(!is.logical(parallel)) {
    cat(paste("Creating cluster of", parallel, "nodes of type", type, "\n"))
    cluster <- snow::makeCluster(parallel, type)

    snow::clusterEvalQ(cluster, library(gridclimind))
    ##snow::clusterEvalQ(cluster, try(getFromNamespace('nc_set_chunk_cache', 'ncdf4')(1024 * 2048, 1009), silent=TRUE))
  }
  cluster
}

#' Create mapping from variables to files.
#'
#' Create mapping from variables to files.
#'
#' Given a variable name map and list of variables in each file, determine a mapping from variables to files.
#'
#' @param variable.name.map A mapping from standardized names (tmax, tmin, prec) to NetCDF variable names.
#' @param v.list A list containing a vector of variables in each file.
#' @return A vector mapping standardized variable names (tmax, tmin, prec) to indices in the file list.
#'
#' @examples
#' \dontrun{
#' ## Get mapping for a single file.
#' input.files <- c("pr_NAM44_CanRCM4_ERAINT_r1i1p1_1989-2009.nc")
#' f <- lapply(input.files, ncdf4::nc_open)
#' v.list <- lapply(f, ncdf4.helpers::nc.get.variable.list, min.dims=2)
#' v.f.idx <- get.var.file.idx(variable.name.map, v.list)
#' }
#'
#' @export
get.var.file.idx <- function(variable.name.map, v.list) {
  v.f.idx <- sapply(variable.name.map, function(v) { which(sapply(v.list, function(vl) { v %in% vl })) }, simplify=FALSE)
  v.f.idx <- unlist(v.f.idx[sapply(v.f.idx, length) > 0])
  return(v.f.idx)
}

## Parallel lapply across 'x', running remote.func, and filtering with local.filter.func .
## Processing is incremental, not batch, to improve parallel throughput and reduce memory consumption.
parLapplyLBFiltered <- function(cl, x, remote.func, ..., local.filter.func=NULL) {
  snow::checkCluster(cl)
  cluster.size <- length(cl)
  num.tasks <- length(x)
  if(num.tasks == 0)
    return(list())
  if(cluster.size == 0)
    stop("Impossible happened; cluster size = 0")

  data.to.return <- vector("list", num.tasks)

  submit.job <- function(cluster.id, task.id) {
    snow::sendCall(cl[[cluster.id]], remote.func, args=c(x[task.id], list(...)), tag=task.id)
  }

  ## Fire off jobs, filling in the cur.task table as we go.
  for(i in 1:min(cluster.size, num.tasks))
    submit.job(i, i)

  next.task <- min(cluster.size, num.tasks)

  ## Stalk and feed jobs
  for(i in 1:num.tasks) {
    d <- snow::recvOneResult(cl)
    next.task <- next.task + 1

    ## Feed the finished node another task if we have one.
    if(next.task <= num.tasks)
      submit.job(d$node, next.task)

    if(!is.null(local.filter.func))
      data.to.return[d$tag] <- list(local.filter.func(d$value, x[[d$tag]]))
    else
      data.to.return[d$tag] <- list(d$value)

    rm(d)
  }

  ## Return data when complete
  return(data.to.return)
}
## need to remove this one
put.history.att <- function(f, v, definemode=FALSE) {
  history.string <- paste("Created by climind", packageVersion("climind"), "on", date())
  ncdf4::ncatt_put(f, v, "history", history.string, definemode=definemode)
  invisible(0)
}

put.ETCCDI.atts <- function(f, freq, orig.title, author.data, definemode=FALSE) {
  ncdf4::ncatt_put(f, 0, "title", paste("climate impact indices computed using E-OBS", author.data$Eobsv, collapse = "-",sep=""), definemode=definemode)
  ncdf4::ncatt_put(f, 0, "EOBS_version", author.data$Eobsv, definemode = definemode)
  ncdf4::ncatt_put(f, 0, "Indices_references", "http://ecad.eu/documents/WCDMP_72_TD_1500_en_1.pdf", definemode=definemode)
  ncdf4::ncatt_put(f, 0, "EOBS_references", "http://www.ecad.eu/download/ensembles/Haylock_et_al_2008.pdf", definemode=definemode)
  ncdf4::ncatt_put(f, 0, "index_calculation_frequency", freq, definemode=definemode)
  ncdf4::ncatt_put(f, 0, "institute", "KNMI", definemode=definemode)
  ncdf4::ncatt_put(f, 0, "webpage", "www.ecad.eu, http://www.ecad.eu/utils/mapserver/eobs_maps_indices.php", definemode=definemode)
  ncdf4::ncatt_put(f, 0, "contact", "eca@knmi.nl", definemode=definemode)
  ncdf4::ncatt_put(f, 0, "file_created", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz="GMT"), definemode=definemode)
  ncdf4::ncatt_put(f, 0, "climind_version", as.character(packageVersion("climind")), definemode=definemode)

  if("institution_id" %in% names(author.data))
    ncdf4::ncatt_put(f, 0, "ETCCDI_institution_id", author.data$institution_id, definemode=definemode)
  if("indices_archive" %in% names(author.data))
    ncdf4::ncatt_put(f, 0, "ETCCDI_indices_archive", author.data$indices_archive, definemode=definemode)
  ## in the future ad github repo
  invisible(0)
}

all.the.same <- function(dat) {
  ifelse(length(dat) == 1, TRUE, all(unlist(lapply(dat, identical, dat[[1]]))))
}
get.split.filename.eobs <- function (eobs.file)
{
  split.path <- strsplit(eobs.file, "/")[[1]]
  fn.split <- strsplit(tail(split.path, n = 1), "_")[[1]]
  names(fn.split) <- c("var", "resolution",
                       "version", "trange", rep(NA, max(0, length(fn.split) - 6)))
  fn.split[length(fn.split)] <- strsplit(fn.split[length(fn.split)],
                                         "\\.")[[1]][1]
  fn.split[c("tstart", "tend")] <- strsplit(fn.split["trange"],
                                            "-")[[1]]
  fn.split
}

custom.nc.make.time.bounds <- function (ts, unit = c("year", "month", "season", "halfyear")) {
  # Based on ncdf4.helpers::nc.make.time.bounds, extended to include season and halfyear

  unit <- match.arg(unit)
  multiplier <- switch(unit, year = 1, month = 12, season = 4, halfyear = 2)
  r <- range(ts)
  r.years <- as.numeric(format(r, "%Y"))
  start.date = switch(unit,
                      year = PCICt::as.PCICt.default(paste(r.years[1], "-01-01", sep = ""), attr(ts, "cal")),
                      month = PCICt::as.PCICt.default(paste(r.years[1], "-01-01", sep = ""), attr(ts, "cal")),
                      season = start.date <- PCICt::as.PCICt.default(paste(r.years[1] - 1, "-12-01", sep = ""), attr(ts, "cal")),    # First season starts 1st of dec of the previous year
                      halfyear = start.date <- PCICt::as.PCICt.default(paste(r.years[1] - 1, "-10-01", sep = ""), attr(ts, "cal")))  # First halfyear starts 1st of okt previous year
  num.years <- r.years[2] - r.years[1] + 1
  seq_unit <- switch(unit, year = '1 year', month = '1 month', season = '3 month', halfyear = '6 month')
  extra_times_at_end = switch(unit, year = 1, month = 1, season = 2, halfyear = 2)
  padded.dates <- seq(start.date, by = seq_unit, length.out = (num.years * multiplier) + extra_times_at_end)
  padded.length <- length(padded.dates)
  bounds <- c(padded.dates[1:(padded.length - 1)], padded.dates[2:padded.length] - 86400)
  dim(bounds) <- c(padded.length - 1, 2)
  t(bounds)
}

get.output.time.data <- function(ts, time.origin.PCICt, time.units, time.dim.name, time.bnds.name, bnds.dim, res=c("year", "month", "season", "halfyear"), origin="1970-01-01") {
  res <- match.arg(res)
  time.bounds <- custom.nc.make.time.bounds(ts, res)
  time.series <- PCICt::as.PCICt.numeric((unclass(time.bounds[1,]) + unclass(time.bounds[2,])) / 2, cal=attr(time.bounds, "cal"), origin=origin)
  time.bounds.days <- as.numeric(julian(time.bounds, origin=time.origin.PCICt))
  time.days <- as.numeric(julian(time.series, origin=time.origin.PCICt))
  time.dim <- ncdf4::ncdim_def(time.dim.name, units=time.units, vals=time.days, unlim=TRUE, longname='')
  time.bnds.var <- ncdf4::ncvar_def(time.bnds.name, '', list(bnds.dim, time.dim), longname='', prec="double")
  return(list(time.dim=time.dim, time.bnds.var=time.bnds.var, time.bnds.data=time.bounds.days))
}

## Get dim sizes, with checking to make sure sizes are all the same.
get.dim.size <- function(f, v.f.idx, variable.name.map) {
  dim.size.list <- lapply(names(v.f.idx), function(i) { f[[v.f.idx[i]]]$var[[variable.name.map[i]]]$varsize })
  stopifnot(all.the.same(dim.size.list))
  dim.size.list[[1]]
}

## Get dim axes, with checking to make sure they all have same axes.
get.dim.axes <- function(f, v.f.idx, variable.name.map) {
  dim.axes.list <- lapply(names(v.f.idx), function(i) { ncdf4.helpers::nc.get.dim.axes(f[[v.f.idx[i]]], variable.name.map[i]) })
  stopifnot(all.the.same(dim.axes.list))
  dim.axes.list[[1]]
}

## Get timeseries (as PCICt), with error checking to ensure input files have same TS.
## FIXME: This will need to be revised for fixed time dimensions. Should be identified by axis.
get.ts <- function(f) {
  ts.list <- lapply(lapply(f, ncdf4.helpers::nc.get.time.series), trunc, "days")
  stopifnot(all.the.same(ts.list))
  ts.list[[1]]
}
#' Flatten the X and Y dimensions down to a space dimension.
#'
#' Flatten the X and Y dimensions down to a space dimension.
#'
#' This function takes input data, a vector of dimensions to reduce to 1 dimension, and optionally a subset of dimnames to copy. It returns the data with the specified dimensions shrunk down to 1 dimension.
#'
#' @param dat The data to operate on.
#' @param reduce.dims The names or indices of the dimensions to reduce to 1 dimension.
#' @param names.subset Optionally, a subset of dimension names to copy.
#' @return The data with the specified dimensions reduced to 1 dimension.
#'
#' @note The dimensions to reduce must be adjoining dimensions.
#'
#' @examples
#' ## Take example data and flatten the last two dims down to one.
#' dat <- structure(1:8, .Dim=c(2, 2, 2))
#' dat.flat <- flatten.dims(dat, 2:3)
#'
#' @export
flatten.dims <- function(dat, reduce.dims, names.subset) {
  stopifnot(all(diff(reduce.dims) == 1))
  dat.dim <- dim(dat)
  if(!missing(names.subset))
    dat.dimnames <- dimnames(dat)
  before.reduce <- 1:length(dat.dim) < min(reduce.dims)
  after.reduce <- 1:length(dat.dim) > max(reduce.dims)
  new.dims <- c(dat.dim[before.reduce], prod(dat.dim[reduce.dims]), dat.dim[after.reduce])
  dim(dat) <- new.dims
  if(!missing(names.subset))
    dimnames(dat) <- dat.dimnames[names.subset]
  return(dat)
}

## FIXME: Handle time-minor data gracefully.
#' Retrieve and convert data to correct units and dimensions.
#'
#' Retrieve and convert data to correct units and dimensions.
#'
#' This function retrieves NetCDF data for the specified subset from the specified file and variable; converts from \code{src.units} to \code{dest.units}, transposes the data to (T, S) dimensionality, and returns the result.
#'
#' @param f The NetCDF file to retrieve data from; an object of class \code{ncdf4}.
#' @param v The variable to retrieve data from.
#' @param subset The subset to retrieve.
#' @param src.units The source units to convert data from.
#' @param dest.units The destination units to convert to.
#' @param dim.axes The dimension axes to be used.
#' @return The retrieved and massaged data.
#'
#' @examples
#' \donttest{get.data(f, "pr", list(Y=3), "kg m-2 s-1", "kg m-2 s-1", c(X="lon",Y="lat",T="time"))}
#'
#' @export
get.data <- function(f, v, subset, src.units, dim.axes) {
  stopifnot(inherits(f, "ncdf4"))
  #dat <- if(!missing(src.units) && !missing(dest.units))
  # udunits2::ud.convert(ncdf4.helpers::nc.get.var.subset.by.axes(f, v, subset), src.units, dest.units)
  #else
  dat <- ncdf4.helpers::nc.get.var.subset.by.axes(f, v, subset)

  reduce.dims <- which(dim.axes %in% c("X", "Y", "Z"))
  return(t(flatten.dims(dat, reduce.dims=reduce.dims)))
}

## Produce slab of northern.hemisphere booleans of the same shape as the data.
#' Determine what portions of a subset are within the northern hemisphere.
#'
#' Determine what portions of a subset are within the northern hemisphere.
#'
#' Given a subset, a file, a variable, and a projection, determine what positions are within the northern hemisphere, returning the result as an array of booleans.
#'
#' @param subset The subset to use.
#' @param f The NetCDF file to use; an object of class \code{ncdf4}.
#' @param v The variable in question.
#' @param projection The proj4 string to use; NULL if the data is not in a projected coordinate space.
#' @return An array of booleans corresponding to the subset containing TRUE if the point is within the northern hemisphere, and FALSE otherwise.
#'
#' @examples
#' \donttest{
#' ## Open files, etc.
#' input.files <- c("tasmax_NAM44_CanRCM4_ERAINT_r1i1p1_1989-2009.nc")
#' f <- list(nc_open(input.files))
#' f.v <- lapply(f, ncdf4.helpers::nc.get.variable.list, min.dims=2)
#' bools <- get.northern.hemisphere.booleans(list(X=1:2, Y=1:2), f[[1]], f.v[[1]], NULL)
#' }
#'
#' @export
get.northern.hemisphere.booleans <- function(subset, f, v, projection) {
  y.dim <- ncdf4.helpers::nc.get.dim.for.axis(f, v, "Y")
  x.dim <- ncdf4.helpers::nc.get.dim.for.axis(f, v, "X")
  y.subset.vals <- rep(y.dim$vals[if(is.null(subset$Y)) 1:y.dim$len else subset$Y],
                       each=(if(is.null(subset$X)) x.dim$len else length(subset$X)))
  if(!is.null(projection)) {
    x.subset.vals <- rep(x.dim$vals[if(is.null(subset$X)) 1:x.dim$len else subset$X],
                         (if(is.null(subset$Y)) y.dim$len else length(subset$Y)))
    dat <- proj4::project(list(x=x.subset.vals, y=y.subset.vals), projection, inverse=TRUE, ellps.default=NA)
    return(dat$y >= 0)
  } else
    return(y.subset.vals >= 0)
}
#' A curry function used only for the Huglin Index
#' This function curries the cdx.funcs so that the current subset (cur_sub) is retrieved with the cdx function
#' It is placed inside compute.indices.for.stripe
#'
curry_in_subset_for_huglin <- function(cdx.funcs, cur_sub){
  cdx.names = names(cdx.funcs)
  cdx.funcs <- lapply(cdx.names, function(function_name) {
    f = cdx.funcs[[function_name]]
    if(grepl('^hi', function_name)) {
      return(functional::Curry(f, cur_sub = cur_sub))
    } else {
      return(f)
    }
  })
  names(cdx.funcs) = cdx.names
  return(cdx.funcs)
}

# #' Get latitude
# #' It is used inside compute.indices.for.stripe
get.lat <- function(open_file_list, variable.name.map) {
  #var.name <- variable.name.map[[names(v.f.idx)[1]]]
  y.dim <- ncdf4.helpers::nc.get.dim.for.axis(open_file_list[[1]], variable.name.map, "Y")
  return(y.dim$vals)
}

#' Creates a list of CMIP5-compliant filenames reflecting the input data.
#'
#' Creates a list of CMIP5-compliant filenames reflecting the input data.
#'
#' This function takes a split filename (as created by \code{get.split.filename.cmip5}) and a list of variables and creates corresponding filenames for the given variables.
#'
#' @param fn.split A vector containing named components, as created by \code{get.split.filename.cmip5}.
#' @param vars.list A vector containing names of variables, as created by \code{\link{get.climdex.variable.list}}.
#' @return A vector containing filenames corresponding to the variables and filename bits supplied.
#'
#' @examples
#' \dontrun{
#' library(ncdf4.helpers)
#' ## Split out filename bits for use below...
#' fn <- "pr_day_BCCAQ+ANUSPLIN300+MRI-CGCM3_historical+rcp85_r1i1p1_19500101-21001231.nc"
#' fn.split <- get.split.filename.cmip5(fn)
#'
#' ## Create filenames with time data and variable appropriately replaced.
#' filenames <- create.climdex.cmip5.filenames(fn.split, c("rx5dayETCCDI_mon", "tn90pETCCDI_yr"))
#' }
#'
#'# create.climdex.cmip5.filenames <- function(fn.split, vars.list) {
#   time.res <- c("yr", "mon")[grepl("_mon$", vars.list) + 1]
#   time.range <- substr(fn.split[c('tstart', 'tend')], 1, 4)
#
#   paste(paste(vars.list, fn.split['model'], fn.split['emissions'], fn.split['run'], sapply(time.res, function(x) { paste(time.range, switch(x, yr=c("", ""), mon=c("01", "12")), sep="", collapse="-") }), sep="_"), ".nc", sep="")
# }
# TODO: this has to change to accomodate seas & halfyears

create.climdex.eobs.filenames <- function(fn.split, vars.list) {
  time.res <- c("yr", "mon")[grepl("_mon$", vars.list) + 1]
  time.range <- substr(fn.split[c('tstart', 'tend')], 1, 4)

  paste(paste(vars.list, fn.split['resolution'], fn.split['version'],sapply(time.res, function(x) { paste(time.range, switch(x, yr=c("", ""), mon=c("01", "12")), sep="", collapse="-") }), sep="_"), ".nc", sep="")
}
