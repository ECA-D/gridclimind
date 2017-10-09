#' Compute Climdex indices for a subset / stripe
#'
#' Compute Climdex indices for a subset / stripe
#'
#' Given a subset, a set of Climdex functions (as created by \code{\link{get.climdex.functions}}), and ancillary data, load and convert data, create a climdexInput object for each point, run all of the functions in \code{cdx.funcs} on that data, and return the result.
#'
#' @param subset The subset to use.
#' @param cdx.funcs The functions to be applied to the data, as created by \code{\link{get.climdex.functions}}.
#' @param ts The associated time data, as created by \code{nc.get.time.series}.
#' @param base.range The base range; a vector of two numeric years.
#' @param dim.axes The dimension axes for the input data.
#' @param v.f.idx A mapping from variables to files, as created by \code{\link{get.var.file.idx}}.
#' @param variable.name.map A mapping from standardized names (tmax, tmin, prec) to NetCDF variable names.
#' @param src.units The source units to convert data from.
#' @param dest.units The destination units to convert to.
#' @param t.f.idx A mapping from threshold variables to threshold files, as created by \code{\link{get.var.file.idx}}.
#' @param thresholds.name.map A mapping from standardized names (tx10thresh, tn90thresh, etc) to NetCDF variable names.
#' @param projection A proj4 string representing the projection the data is in.
#' @param f A list of objects of type \code{ncdf4}, consisting of the open input files. If missing, will be pulled from the global namespace.
#' @param thresholds.netcdf A list of objects of type \code{ncdf4}, consisting of the open threshold files. If missing, will be pulled from the global namespace.
#'
#' @note This function relies on an object named 'f' and containing the opened NetCDF files being part of the global namespace.
#'
#' @examples
#' \donttest{
#' ## Define mappings and filenames.
#' author.data <- list(institution="Looney Bin", institution_id="LBC")
#' input.files <- c("pr_NAM44_CanRCM4_ERAINT_r1i1p1_1989-2009.nc")
#' variable.name.map <- c(tmax="tasmax", tmin="tasmin", prec="pr")
#'
#' ## Open files, etc.
#' cdx.funcs <- get.climdex.functions(get.climdex.variable.list("tmax"))
#' f <- lapply(input.files, ncdf4::nc_open)
#' f.meta <- create.file.metadata(f, variable.name.map)
#' climdex.var.list <- get.climdex.variable.list(names(f.meta$v.f.idx), "all", NULL)
#' cdx.meta <- get.climdex.variable.metadata(climdex.var.list, input.files[1])
#'
#' ## Compute indices for stripe
#' cdx <- compute.indices.for.stripe(list(Y=1), cdx.funcs, f.meta$ts, c(1981, 1990), f.meta$dim.axes,
#'                            f.meta$v.f.idx, variable.name.map, f.meta$src.units, f.meta$dest.units,
#'                            t.f.idx, NULL, f=f, thresholds.netcdf=NULL)
#' }
#'
#' @export
compute.indices.for.stripe <- function(subset, metadata.config, cdx.funcs, ts, base.range, dim.axes, v.f.idx, variable.name.map, src.units, t.f.idx, thresholds.name.map,
                                       projection=NULL, f, thresholds.netcdf) {
  cat('We are now at latitude', subset[['Y']], '\n')

  f <- if(missing(f)) get("f", .GlobalEnv) else f
  thresholds.netcdf <- if(missing(thresholds.netcdf)) get("thresholds.netcdf", .GlobalEnv) else thresholds.netcdf

  ## Dimension order: Time, Space for each Var in list
  data.list <- sapply(names(v.f.idx), function(x) { gc(); get.data(f[[v.f.idx[x]]], variable.name.map[x], subset, src.units[x], dim.axes) }, simplify=FALSE)
  gc()

  northern.hemisphere <- get.northern.hemisphere.booleans(subset, f[[v.f.idx[1]]], variable.name.map[names(v.f.idx)[1]], projection)

  # Curry the current subset (latitude) into the Huglin function
  # this needs to be done on the fly, because only in this context do we really
  # know what the latitude is.
  latitudes = get.lat(f, v.f.idx, variable.name.map)
  cur_sub <- latitudes[subset[['Y']]]
  cdx.funcs <- curry_in_subset_for_huglin(cdx.funcs, cur_sub)

  thresholds <- if(is.null(thresholds.netcdf)) NULL else get.thresholds.chunk(subset, cdx.funcs, thresholds.netcdf, t.f.idx, thresholds.name.map)
  return(lapply(1:(dim(data.list[[1]])[2]), function(x) {
    dat.list <- sapply(names(data.list), function(name) { data.list[[name]][,x] }, simplify=FALSE)
    ## Fast-path the all-NA case.
    if(all(sapply(dat.list, function(x) { all(is.na(x)) }))) {
      ## We don't need to pad this out to full length; cbind will do that for us.
      return(structure(as.list(rep(NA, length(cdx.funcs))), .Names=names(cdx.funcs)))
    } else {
      indices.input <- c(dat.list, northern.hemisphere=northern.hemisphere[x], list(quantiles=get.quantiles.object(thresholds, x, metadata.config)))
      return(compute.climdex.indices(indices.input, cdx.funcs, ts, base.range))
    }
  }))
}
