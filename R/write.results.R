## Write out results for variables computed
#' Write out computed climdex results
#'
#' Write out computed climdex results
#'
#' Given a set of Climdex results, a subset, a set of files, and dimension sizes, write out the data to the appropriate files.
#'
#' @param climdex.results The results to write out.
#' @param chunk.subset The corresponding subset.
#' @param cdx.ncfile The list of NetCDF files to write the results out to.
#' @param dim.size The overall size of the input data.
#' @param cdx.varname The list of NetCDF variable names for the files in \code{cdx.ncfile}.
#'
#' @examples
#' \donttest{
#' ## Define mappings and filenames.
#' author.data <- list(institution="Looney Bin", institution_id="LBC")
#' input.files <- c("pr_NAM44_CanRCM4_ERAINT_r1i1p1_1989-2009.nc")
#' variable.name.map <- c(tmax="tasmax", tmin="tasmin", prec="pr")
#'
#' ## Open files, etc.
#' cdx.funcs <- get.climdex.functions("tmax")
#' f <- lapply(input.files, ncdf4::nc_open)
#' f.meta <- create.file.metadata(f, variable.name.map)
#' climdex.var.list <- get.climdex.variable.list(names(f.meta$v.f.idx), "all", NULL)
#' cdx.meta <- get.climdex.variable.metadata(climdex.var.list, input.files[1])
#'
#' ## Create output files
#' cdx.ncfile <- create.ncdf.output.files(cdx.meta, f, f.meta$v.f.idx, variable.name.map,
#'                                        f.meta$ts, get.time.origin(f, f.meta$dim.axes),
#'                                        c(1981,1990), "/foo", author.data)
#'
#' ## Compute indices for stripe
#' cdx <- compute.indices.for.stripe(list(Y=1), cdx.funcs, f.meta$ts, c(1991, 2000), f.meta$dim.axes,
#'                            f.meta$v.f.idx, variable.name.map, f.meta$src.units, f.meta$dest.units,
#'                            t.f.idx, NULL, f=f, thresholds.netcdf=NULL)
#'
#' ## Write out indices
#' write.climdex.results(cdx, list(Y=1), cdx.ncfile, f.meta$dim.size, cdx.meta$varname)
#' }
#'
#' @export
write.climdex.results <- function(climdex.results, chunk.subset, cdx.ncfile, dim.size, cdx.varname) {
  xy.dims <- dim.size[1:2]
  if(!is.null(chunk.subset$X))
    xy.dims[1] <- length(chunk.subset$X)
  if(!is.null(chunk.subset$Y))
    xy.dims[2] <- length(chunk.subset$Y)

  ## Write out results, variable by variable
  lapply(1:length(cdx.ncfile), function(v) {
    dat <- t(do.call(cbind, lapply(climdex.results, function(cr) { cr[[v]] })))
    t.dim.len <- ncdf4.helpers::nc.get.dim.for.axis(cdx.ncfile[[v]], cdx.varname[v], "T")$len

    ## If data is of length 1, it's an error.
    if(length(dat) == 1)
      stop(dat)

    ## Special case of an entire slab missing values... repeat such that we have full data.
    if(prod(dim(dat)) != prod(c(xy.dims, t.dim.len)))
      dat <- rep(dat, t.dim.len)

    dim(dat) <- c(xy.dims, t.dim.len)
    ncdf4.helpers::nc.put.var.subset.by.axes(cdx.ncfile[[v]], cdx.varname[v], dat, chunk.subset)
  })
  invisible(0)
}
