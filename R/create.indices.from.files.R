## Run Climdex and populate the output files
#' Create Climdex indices from NetCDF input files.
#'
#' Create Climdex indices from NetCDF input files.
#'
#' This function computes Climdex indices from NetCDF input files, writing out one file per variable named like the \code{template.filename}, which must follow the CMIP5 file naming conventions (this is a deficiency which will be corrected in later versions).
#'
#' The indices to be calculated can be specified; if not, they will be determined by data availability. Thresholds can be supplied (via \code{thresholds.files}) or, if there is data within the base period, calculated and used as part of the process. Note that in-base thresholds are separate from out-of-base thresholds; this is covered in more detail in the help for the \code{climind} package.
#'
#' The metadata is stored in JSON files that are included with the pacakge. Right now, the metadata relevant to EOBS is used by default. To switch to another set of metadata, use the \code{metadata.id}
#' global option:
#'
#'     \code{options(metadata.id = 'eobs')}
#'
#' Note that currently only EOBS metadata is available (\code{metadata.id = 'eobs'}).
#'
#' @param input.files A list of filenames of NetCDF files to be used as input. A NetCDF file may contain one or more variables.
#' @param out.dir The directory to put the output files in.
#' @param output.filename.template The output filename to be used as a template, which must follow the CMIP5 file naming conventions.
#' @param author.data Data describing the author; a character vector with 0 or more of the following named values:\describe{
#' \item{institution}{The institution generating the data.}
#' \item{institution_id}{An abbreviation for the institution generating the data.}
#' \item{indices_archive}{The URL the data is published at, if applicable.}
#' \item{contact}{The email address or contact info for the author.}
#' \item{references}{What to reference when citing this work.}
#' }
#' @param climdex.vars.subset A character vector of lower-case names of Climdex indices to calculate (eg: tr, fd, rx5day). See the list of 27 indices in the References section.
#' @param climdex.time.resolution The time resolution to compute indices at; one of "all" (both monthly and annual), "annual" (only annual), or "monthly" (only monthly).
#' @param axis.to.split.on The axis to split up the data on for parallel / incremental processing.
#' @param fclimdex.compatible Whether the thresholds should be created to match fclimdex thresholds; affects padding at the ends of the base period.
#' @param base.range Vector of two numeric years specifying the start and end years.
#' @param parallel The number of parallel processing threads, or FALSE if no parallel processing is desired.
#' @param verbose Whether to be chatty.
#' @param thresholds.files A character vector of files containing thresholds to be used.
#' @param max.vals.millions The number of data values to process at one time (length of time dim * number of values * number of variables).
#' @param cluster.type The cluster type, as used by the \code{snow} library.
#'
#' @note NetCDF input files may contain one or more variables, named as per \code{variable.name.map} in the json config file. The code will search the files for the named variables. The same is true of thresholds files; one file may be supplied, or multiple files may be supplied, via the \code{thresholds.files} argument; and the name mapping may be supplied via the \code{thresholds.name.map} argument.
#'
#' @references \url{http://etccdi.pacificclimate.org/list_27_indices.shtml}
#' @examples
#' \dontrun{
#' ## Prepare input data and calculate indices for a single file
#' ## with a single thread (no parallelism).
#' input.files <- c("pr_NAM44_CanRCM4_ERAINT_r1i1p1_1989-2009.nc")
#' author.data <- list(institution="Looney Bin", institution_id="LBC")
#' create.indices.from.files(input.files, "out_dir/", input.files[1], author.data,
#'                           base.range=c(1991, 2000), parallel=FALSE)
#'
#' ## Prepare input data and calculate indices for two files
#' ## in parallel given thresholds.
#' input.files <- c("pr_NAM44_CanRCM4_ERAINT_r1i1p1_1989-2009.nc",
#'                  "tasmax_NAM44_CanRCM4_ERAINT_r1i1p1_1989-2009.nc")
#' author.data <- list(institution="Looney Bin", institution_id="LBC")
#' create.indices.from.files(input.files, "out_dir/", input.files[1], author.data,
#'                           base.range=c(1991, 2000), parallel=8, thresholds.files="thresh.nc")
#' }
#'
#' @export
create.indices.from.files <- function(input.files, out.dir, output.filename.template, author.data, climdex.vars.subset=NULL, climdex.time.resolution=c("all", "annual", "monthly", "seasonal", "halfyear"),
                                      axis.to.split.on="Y", fclimdex.compatible=TRUE, base.range=c(1961, 1990),
                                      parallel=4, verbose=FALSE, thresholds.files=NULL, max.vals.millions=20, cluster.type="SOCK") {
  if(!(is.logical(parallel) || is.numeric(parallel)))
    stop("'parallel' option must be logical or numeric.")

  if(length(input.files) == 0)
    stop("Require at least one input file.")

  ## Load a json config file that contains the majority of the configurable options, e.g. long name, etc
  metadata.config = read_json_metadata_config_file()
  variable.name.map = metadata.config$get.variable.name.map()
  thresholds.name.map = metadata.config$get.thresholds.name.map()

  ## Open files, determine mapping between files and variables.
  f <- lapply(input.files, ncdf4::nc_open)
  f.meta <- create.file.metadata(f, variable.name.map)

  ## Get thresholds variable-file mapping
  t.f.idx <- get.thresholds.f.idx(thresholds.files, thresholds.name.map)

  ## Get variable list, subset if necessary
  climdex.time.resolution <- match.arg(climdex.time.resolution)
  climdex.var.list <- get.climdex.variable.list(names(f.meta$v.f.idx), metadata.config, climdex.time.resolution, climdex.vars.subset)

  cdx.meta <- get.climdex.variable.metadata(climdex.var.list, output.filename.template, metadata.config)
  cdx.ncfile <- create.ncdf.output.files(cdx.meta, f, f.meta$v.f.idx, variable.name.map, f.meta$ts, get.time.origin(f, f.meta$dim.axes), base.range, out.dir, author.data, metadata.config)
  cdx.funcs <- get.climdex.functions(climdex.var.list, metadata.config, fclimdex.compatible = fclimdex.compatible)

  ## Compute indices, either single process or multi-process using 'parallel'
  subsets <- ncdf4.helpers::get.cluster.worker.subsets(max.vals.millions * 1000000, f.meta$dim.size, f.meta$dim.axes, axis.to.split.on)
  if(is.numeric(parallel)) {
    ## Setup...
    lapply(f, ncdf4::nc_close)
    rm(f)
    cluster <- set.up.cluster(parallel, cluster.type)
    snow::clusterExport(cluster, list("input.files", "thresholds.files"), environment())
    snow::clusterEvalQ(cluster, f <<- lapply(input.files, ncdf4::nc_open, readunlim=FALSE))
    snow::clusterEvalQ(cluster, thresholds.netcdf <<- thresholds.open(thresholds.files))

    ## Meat...
    parLapplyLBFiltered(cluster, subsets, compute.indices.for.stripe, metadata.config, cdx.funcs, f.meta$ts, base.range, f.meta$dim.axes, f.meta$v.f.idx, variable.name.map, f.meta$src.units, t.f.idx, thresholds.name.map, f.meta$projection, local.filter.func=function(x, x.sub) {
      write.climdex.results(x, x.sub, cdx.ncfile, f.meta$dim.size, cdx.meta$var.name)
    })

    ## Clean-up.
    snow::stopCluster(cluster)
  } else {
    ## Setup...
    thresholds.netcdf <- thresholds.open(thresholds.files)
    ##try(getFromNamespace('nc_set_chunk_cache', 'ncdf4')(1024 * 2048, 1009), silent=TRUE)

    ## Meat...
    lapply(subsets, function(x) { write.climdex.results(compute.indices.for.stripe(x, metadata.config, cdx.funcs, f.meta$ts, base.range, f.meta$dim.axes, f.meta$v.f.idx, variable.name.map, f.meta$src.units, t.f.idx, thresholds.name.map, f.meta$projection, f, thresholds.netcdf), x, cdx.ncfile, f.meta$dim.size, cdx.meta$var.name) })

    ## Clean-up.
    thresholds.close(thresholds.netcdf)
    lapply(f, ncdf4::nc_close)
  }

  ## Close all the output files
  lapply(cdx.ncfile, ncdf4::nc_close)

  cat("Finished computing indices\n")
  invisible(0)
}
