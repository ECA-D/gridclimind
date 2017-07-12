#' Retrieve metadata about NetCDF-format files.
#'
#' Retrieve metadata about NetCDF-format files.
#'
#' Given a list of NetCDF files and a mapping from standard variable names (tmax, tmin, prec) to NetCDF variable names, retrieve a set of standardized metadata.
#'
#' @param f The list of NetCDF files.
#' @param variable.name.map A named character vector mapping standard variable names (tmax, tmin, prec) to NetCDF variable names.
#' @return A list containing time data (ts), dimension sizes (dim.size), dimension axes (dim.axes), source units (src.units), destination units (dest.units), a mapping from variables to files (v.f.idx), and a projection, if available.
#'
#' @examples
#' \dontrun{
#' ## Get metadata about a single input file.
#' input.files <- c("pr_NAM44_CanRCM4_ERAINT_r1i1p1_1989-2009.nc")
#' f <- lapply(input.files, ncdf4::nc_open)
#' f.meta <- create.file.metadata(f, variable.name.map)
#' }
#'
#' @export
create.file.metadata <- function(f, variable.name.map) {
  v.list <- lapply(f, ncdf4.helpers::nc.get.variable.list, min.dims=2)
  v.f.idx <- get.var.file.idx(variable.name.map, v.list)

  if(any(sapply(v.list, function(vl) { sum(variable.name.map %in% vl) }) == 0))
    stop("At least one input file doesn't contain any of the named variables.")
  if(anyDuplicated(unlist(names(v.f.idx))))
    stop("Variable(s) present in more than one input file.")

  ## Get units and specify destination units
  dest.units <- c(prec="mm", tmax="degrees_C", tmin="degrees_C", tavg="degrees_C")
  dest.units <- dest.units[names(dest.units) %in% names(v.f.idx)]

  ## Get projection
  projection <- ncdf4.helpers::nc.get.proj4.string(f[[1]], v.list[[1]][1])
  stopifnot(!is.null(projection))
  if(projection == "")
    projection <- NULL

  return(list(ts=get.ts(f), dim.size=get.dim.size(f, v.f.idx, variable.name.map), dim.axes=get.dim.axes(f, v.f.idx, variable.name.map),
              src.units=sapply(names(v.f.idx), function(i) { f[[v.f.idx[i]]]$var[[variable.name.map[i]]]$units }),
              dest.units=dest.units, v.f.idx=v.f.idx, projection=projection))
}
