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

  r <- 1:(dim(data.list[[1]])[2])
  if(!is.null(data.list$tmax)) {
    if(!is.null(data.list$tmin)) {
      if(!is.null(data.list$prec)) {
        return(lapply(r, function(x) climind::get.outofbase.quantiles(data.list$tmax[,x], data.list$tmin[,x], data.list$prec[,x], ts, ts, ts, base.range)))
      } else {
        return(lapply(r, function(x) climind::get.outofbase.quantiles(data.list$tmax[,x], data.list$tmin[,x], NULL, ts, ts, NULL, base.range)))
      }
    } else {
      if(!is.null(data.list$prec)) {
        return(lapply(r, function(x) climind::get.outofbase.quantiles(data.list$tmax[,x], NULL, data.list$prec[,x], ts, NULL, ts, base.range)))
      } else {
        return(lapply(r, function(x) climind::get.outofbase.quantiles(data.list$tmax[,x], NULL, NULL, ts, NULL, NULL, base.range)))
      }
    }
  } else {
    if(!is.null(data.list$tmin)) {
      if(!is.null(data.list$prec)) {
        return(lapply(r, function(x) climind::get.outofbase.quantiles(NULL, data.list$tmin[,x], data.list$prec[,x], NULL, ts, ts, base.range)))
      } else {
        return(lapply(r, function(x) climind::get.outofbase.quantiles(NULL, data.list$tmin[,x], NULL, NULL, ts, NULL, base.range)))
      }
    } else {
      if(!is.null(data.list$prec)) {
        return(lapply(r, function(x) climind::get.outofbase.quantiles(NULL, NULL, data.list$prec[,x], NULL, NULL, ts, base.range)))
      } else {
        stop("Go home and take your shitty input with you.")
      }
    }
  }
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
get.quantiles.object <- function(thresholds, idx) {
  if(is.null(thresholds))
    return(NULL)

  thresh.path.2d <- list(tx10thresh=c("tmax", "outbase", "q10"),
                         tx90thresh=c("tmax", "outbase", "q90"),
                         tn10thresh=c("tmin", "outbase", "q10"),
                         tn90thresh=c("tmin", "outbase", "q90"))
  thresh.path.1d <- list(r75thresh=c("prec", "q75"),
                         r95thresh=c("prec", "q95"),
                         r99thresh=c("prec", "q99"))
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
