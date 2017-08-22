## Run Climdex to generate indices for computing Climdex on future data
#' Create Climdex thresholds used for computing threshold-based indices
#'
#' Create Climdex thresholds used for computing threshold-based indices
#'
#' For many applications, one may want to compute thresholds on one data set, then apply them to another. This is usually the case when comparing GCM (Global Climate Model) results for future time periods to either historical reanalysis data or historical / pre-industrial control runs from models. The purpose of this function is to compute these thresholds on the data supplied, saving them to the file specified. Then these thresholds can be used with \code{\link{create.indices.from.files}} to compute indices using the thresholds computed using this code.
#'
#' @param input.files A list of filenames of NetCDF files to be used as input. A NetCDF file may contain one or more variables.
#' @param output.file The name of the file to be created.
#' @param author.data A vector containing named elements describing the author; see \code{\link{create.indices.from.files}}.
#' @param variable.name.map A character vector mapping from standardized names (tmax, tmin, prec) to NetCDF variable names.
#' @param axis.to.split.on The axis to split up the data on for parallel / incremental processing.
#' @param fclimdex.compatible Whether the thresholds should be created to match fclimdex thresholds; affects padding at the ends of the base period.
#' @param base.range Vector of two numeric years specifying the start and end years.
#' @param parallel The number of parallel processing threads, or FALSE if no parallel processing is desired.
#' @param verbose Whether to be chatty.
#' @param max.vals.millions The number of data values to process at one time (length of time dim * number of values * number of variables).
#' @param cluster.type The cluster type, as used by the \code{snow} library.
#'
#' @note NetCDF input files may contain one or more variables, named as per \code{variable.name.map}. The code will search the files for the named variables.
#'
#' @examples
#' \dontrun{
#' ## Prepare input data and calculate thresholds for file.
#' input.files <- c("pr_NAM44_CanRCM4_ERAINT_r1i1p1_1989-2009.nc")
#' author.data <- list(institution="Looney Bin", institution_id="LBC")
#' create.thresholds.from.file(input.files, "thresh.nc", author.data,
#'                             base.range=c(1991, 2000), parallel=FALSE)
#' }
#'
#' @export
create.thresholds.from.file <- function(input.files, output.file, author.data, variable.name.map=c(tmax="tx", tmin="tn", prec="rr", tavg="tg"),
                                        axis.to.split.on="Y", fclimdex.compatible=TRUE, base.range=c(1961, 1990), parallel=4, verbose=FALSE, max.vals.millions=20, cluster.type="SOCK") {
  if(!(is.logical(parallel) || is.numeric(parallel)))
    stop("'parallel' option must be logical or numeric.")

  if(length(input.files) == 0)
    stop("Require at least one input file.")

  ## Load a json config file that contains the majority of the configurable options, e.g. long name, etc
  metadata.config = read_json_metadata_config_file()

  f <- lapply(input.files, ncdf4::nc_open)
  f.meta <- create.file.metadata(f, variable.name.map)

  ## Define what the threshold indices will look like...
  threshold.dat <- get.thresholds.metadata(names(f.meta$v.f.idx), metadata.config)

  ## Create the output file
  thresholds.netcdf <- create.thresholds.file(output.file, f, f.meta$ts, f.meta$v.f.idx, variable.name.map, base.range, f.meta$dim.size, f.meta$dim.axes, threshold.dat, author.data)

  cluster <- set.up.cluster(parallel, cluster.type)
  subsets <- ncdf4.helpers::get.cluster.worker.subsets(max.vals.millions * 1000000, f.meta$dim.size, f.meta$dim.axes, axis.to.split.on)

  write.thresholds.data <- function(out.list, out.sub) {
    lapply(names(threshold.dat), function(n) {
      d <- threshold.dat[[n]]
      if(d$has.time)
        dat <- t(sapply(out.list, function(y) { return(y[[d$q.path]]) }))
      else
        dat <- sapply(out.list, function(y) { return(y[[d$q.path[1]]][d$q.path[2]]) })
      dim(dat) <- unsquash.dims(dim(dat), out.sub, thresholds.netcdf, n)
      ncdf4.helpers::nc.put.var.subset.by.axes(thresholds.netcdf, n, dat, out.sub)
    })
    gc()
  }

  if(!is.null(cluster)) {
    lapply(f, ncdf4::nc_close)
    rm(f)

    snow::clusterExport(cluster, "input.files", environment())
    snow::clusterEvalQ(cluster, f <<- lapply(input.files, ncdf4::nc_open, readunlim=FALSE))

    ## Compute subsets and fire jobs off; collect and write out chunk-at-a-time
    parLapplyLBFiltered(cluster, subsets, get.quantiles.for.stripe, f.meta$ts, base.range, f.meta$dim.axes, f.meta$v.f.idx, variable.name.map, f.meta$src.units, local.filter.func=write.thresholds.data)

    snow::stopCluster(cluster)
  } else {
    ##try(getFromNamespace('nc_set_chunk_cache', 'ncdf4')(1024 * 2048, 1009), silent=TRUE)

    lapply(subsets, function(x) { write.thresholds.data(get.quantiles.for.stripe(x, f.meta$ts, base.range, f.meta$dim.axes, f.meta$v.f.idx, variable.name.map, f.meta$src.units, f), x) })

    lapply(f, ncdf4::nc_close)
  }

  ## Close all the files
  ncdf4::nc_close(thresholds.netcdf)

  cat("Finished computing thresholds\n")
  invisible(0)
}

#' Open thresholds file(s)
#'
#' Open thresholds file(s)
#'
#' This function opens one or more thresholds files and returns the \code{ncdf4} objects as a list.
#'
#' @param thresholds.files A character vector containing the names of thresholds files.
#' @return A list of objects of class \code{ncdf4}, or NULL if thresholds.files is NULL.
#'
#' @examples
#' \dontrun{
#' ## Open a single thresholds file
#' thresholds.files <- c("thresh.nc")
#' thresh <- thresholds.open(thresholds.files)
#' }
#'
#' @export
thresholds.open <- function(thresholds.files) {
  return(if(is.null(thresholds.files)) NULL else lapply(thresholds.files, ncdf4::nc_open))
}

#' Close thresholds file(s)
#'
#' Close thresholds file(s)
#'
#' This function closes one or more thresholds files.
#'
#' @param thresholds.nc A list of objects of class \code{ncdf4}, or NULL
#'
#' @examples
#' \dontrun{
#' ## Open a single thresholds file, then close it.
#' thresholds.files <- c("thresh.nc")
#' thresh <- thresholds.open(thresholds.files)
#' thresholds.close(thresh)
#' }
#'
#' @export
thresholds.close <- function(thresholds.nc) {
  if(!is.null(thresholds.nc)) lapply(thresholds.nc, ncdf4::nc_close)
  invisible(0)
}

#' Retrieve threshold metadata
#'
#' Retrieve threshold metadata
#'
#' Returns units, long names, locations within the t data structure, and whether time data should be included given the variable information available.
#'
#' @param var.names A vector containing names of available variables (tmax, tmin, prec).
#' @return A list containing metadata for each of the six thresholds.
#'
#' @examples
#' thresholds.meta <- get.thresholds.metadata("prec")
#'
#' @export
get.thresholds.metadata <- function(var.names, metadata.config) {
  threshold.dat = metadata.config$get.thresholds.metadata()
  return(threshold.dat[sapply(threshold.dat, function(x) { x$q.path[1] %in% var.names })])
}

get.thresholds.f.idx <- function(thresholds.files, thresholds.name.map) {
  if(is.null(thresholds.files)) {
    return(NULL)
  } else {
    thresh <- thresholds.open(thresholds.files)
    t.f.idx <- get.var.file.idx(thresholds.name.map, lapply(thresh, ncdf4.helpers::nc.get.variable.list, min.dims=2))
    thresholds.close(thresh)
    return(t.f.idx)
  }
}

#' Retrieve thresholds for a subset
#'
#' Retrieve thresholds for a subset
#'
#' Given a subset, a set of Climdex functions (as created by \code{\link{get.climdex.functions}}), and ancillary data, load the thresholds required for the functions being called and return them.
#'
#' @param subset The subset to use.
#' @param cdx.funcs The functions to be applied to the data, as created by \code{\link{get.climdex.functions}}.
#' @param thresholds.netcdf One or more NetCDF files containing thresholds.
#' @param t.f.idx A mapping from threshold variables to threshold files, as created by \code{\link{get.var.file.idx}}.
#' @param thresholds.name.map A mapping from standardized names (tx10thresh, tn90thresh, etc) to NetCDF variable names.
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
#' }
#'
#' @export
get.thresholds.chunk <- function(subset, cdx.funcs, thresholds.netcdf, t.f.idx, thresholds.name.map) {
  var.thresh.map <- list(tx10thresh=c("tx10p"), tx90thresh=c("tx90p", "WSDI"), tn10thresh=c("tn10p", "CSDI"),
                         tn90thresh=c("tn90p"), r75thresh=c("r75p"), r95thresh=c("r95p"), r99thresh=c("r99p"))

  cdx.names <- names(cdx.funcs)
  thresh.var.needed <- names(var.thresh.map)[sapply(var.thresh.map, function(x) { any(unlist(lapply(x, function(substr) { any(grepl(substr, cdx.names)) }))) })]
  stopifnot(all(thresh.var.needed %in% names(t.f.idx)))
  return(sapply(thresh.var.needed, function(threshold.var) {
    dim.axes <- ncdf4.helpers::nc.get.dim.axes(thresholds.netcdf[[t.f.idx[threshold.var]]], thresholds.name.map[threshold.var]);
    return(get.data(thresholds.netcdf[[t.f.idx[threshold.var]]], thresholds.name.map[threshold.var], subset, dim.axes=dim.axes))
  }, simplify=FALSE))
}
