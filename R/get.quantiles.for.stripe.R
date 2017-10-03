#' Compute Climdex thresholds for a subset / stripe
#'
#' Compute Climdex thresholds for a subset / stripe
#'
#' Given a subset and ancillary data, load and convert data, get the out-of-base quantiles for the data for each point, and return the result.
#'
#' @param subset The subset to use.
#' @param ts The associated time data, as created by \code{nc.get.time.series}.
#' @param base.range The base range; a vector of two numeric years.
#' @param dim.axes The dimension axes for the input data.
#' @param v.f.idx A mapping from variables to files, as created by \code{\link{get.var.file.idx}}.
#' @param variable.name.map A mapping from standardized names (tmax, tmin, prec) to NetCDF variable names.
#' @param src.units The source units to convert data from.
#' @param dest.units The destination units to convert to.
#' @param f A list of objects of type \code{ncdf4}, consisting of the open input files. If missing, will be pulled from the global namespace.
#'
#' @note This function relies on an object named 'f' and containing the opened NetCDF files being part of the global namespace.
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
#'                                       c(1991, 2000), f.meta$dim.size, f.meta$dim.axes,
#'                                       threshold.dat, author.data)
#'
#' ## Compute threshold quantiles for stripe
#' q <- get.quantiles.for.stripe(list(Y=1), f.meta$ts, c(1991, 2000), f.meta$dim.axes,
#'                               f.meta$v.f.idx, variable.name.map, f.meta$src.units,
#'                               f.meta$dest.units, f)
#' }
#'
#' @export
get.quantiles.for.stripe <- function(subset, ts, base.range, dim.axes, v.f.idx, variable.name.map, src.units, f) {
  f <- if(missing(f)) get("f", .GlobalEnv) else f
  data.list <- sapply(names(v.f.idx), function(x) { gc(); get.data(f[[v.f.idx[x]]], variable.name.map[x], subset, src.units[x], dim.axes) }, simplify=FALSE)
  gc()

  # Constuct input arguments for quantile calculation function
  potential_data = lapply(names(variable.name.map), function(var) data.list[[var]])
  names(potential_data) = names(variable.name.map)
  ts_arguments = lapply(potential_data, function(x) {
    if (is.null(x)) {
      return(NULL)
    } else {
      return(ts)
    }
  })
  names(ts_arguments) = paste(names(ts_arguments), 'dates', sep = '.')
  quantile_input_arguments = c(potential_data, ts_arguments, list(base.range = base.range))

  r <- 1:(dim(data.list[[1]])[2])
  return(lapply(r, function(x) {
    concrete_args = quantile_input_arguments
    concrete_args$tmax = concrete_args$tmax[,x]
    concrete_args$tmin = concrete_args$tmin[,x]
    concrete_args$tavg = concrete_args$tavg[,x]
    concrete_args$prec = concrete_args$prec[,x]
    return(do.call(climind::get.outofbase.quantiles, concrete_args, quote = TRUE))
  }))
}

#' Extract a single quantiles object from a set of thresholds.
#'
#' Extract a single quantiles object from a set of thresholds.
#'
#' From a set of thresholds as retrieved from one or more NetCDF files containing thresholds, this function extracts a single point and converts the format to one suitable for passing to \code{climdexInput.raw}.
#'
#' @param thresholds The thresholds, as extracted by \code{\link{get.thresholds.chunk}}.
#' @param idx The index to extract.
#' @return A quantiles object suitable for passing to \code{climdexInput.raw} as the \code{quantiles} argument.
#'
#' @examples
#' \donttest{
#' ## Define mappings and filenames.
#' thresholds.name.map <- c(tx10thresh="tx10thresh", tn10thresh="tn10thresh", tx90thresh="tx90thresh",
#'                          tn90thresh="tn90thresh", r95thresh="r95thresh", r99thresh="r99thresh")
#' thresh.files <- "thresholds.nc"
#'
#' ## Open files, etc.
#' cdx.funcs <- get.climdex.functions(get.climdex.variable.list("tmax"))
#' thresholds.netcdf <- lapply(thresh.files, nc_open)
#' t.f.idx <- get.var.file.idx(thresholds.name.map, lapply(thresholds.netcdf,
#'                             ncdf4.helpers::nc.get.variable.list, min.dims=2))
#'
#' ## Get thresholds chunk.
#' dat <- get.thresholds.chunk(list(Y=1), cdx.funcs, thresholds.netcdf, t.f.idx, thresholds.name.map)
#'
#' ## Get quantiles object for index 2
#' q <- get.quantiles.object(dat, 2)
#' }
#'
#' @export
get.quantiles.object <- function(thresholds, idx, metadata.config) {
  if(is.null(thresholds))
    return(NULL)

  thresh.path = metadata.config$get.threshold.path()
  thresh.path.2d <- thresh.path[['1d']]
  thresh.path.1d <- thresh.path[['2d']]
  result <- list()


  recursive.append <- function(x, l, data) {
    if(length(x) == 0) return(data)
    if(is.null(l)) l <- list()
    return(c(l[!(names(l) %in% x[1])], structure(list(recursive.append(tail(x, n=-1), l[[x[1]]], data)), .Names=x[1])))
  }


  for(threshold.var in names(thresh.path.2d)[names(thresh.path.2d) %in% names(thresholds)])
    result <- recursive.append(thresh.path.2d[[threshold.var]], result, thresholds[[threshold.var]][,idx])

  for(threshold.var in names(thresh.path.1d)[names(thresh.path.1d) %in% names(thresholds)]) {
    thresh.path <- thresh.path.1d[[threshold.var]]
    result[[thresh.path[1]]] <- c(result[[thresh.path[1]]], structure(thresholds[[threshold.var]][idx], .Names=thresh.path[2]))
  }

  return(result)
}
