## Run to estimate UTCI
#' Create Climdex UTCI
#'
#' Create Climdex UTCI
#'
#' The purpose of this function is to compute UTCI on the data supplied, saving them to the file specified.
#'
#' The metadata is stored in JSON files that are included with the pacakge. Right now, the metadata relevant to EOBS is used by default. To switch to another set of metadata, use the \code{metadata.id}
#' global option:
#'
#'     \code{options(metadata.id = 'eobs')}
#'
#' Note that currently only EOBS metadata is available (\code{metadata.id = 'eobs'}).
#'
#' @param input.files A list of filenames of NetCDF files to be used as input. A NetCDF file may contain one or more variables.
#' @param output.file The name of the file to be created.
#' @param author.data A vector containing named elements describing the author; see \code{\link{create.indices.from.files}}.
#' @param axis.to.split.on The axis to split up the data on for parallel / incremental processing.
#' @param parallel The number of parallel processing threads, or FALSE if no parallel processing is desired.
#' @param verbose Whether to be chatty.
#'
#' @param cluster.type The cluster type, as used by the \code{snow} library.
#'
#' @note NetCDF input files may contain one or more variables, named as per \code{variable.name.map} (read from config json file). The code will search the files for the named variables.
#'
#' @examples
#' \dontrun{
#' ## Prepare input data and calculate utci for file.
#' input.files <- c(paste0(in.dir,"tn_0.50deg_regular_1979-2018.nc"),
#' paste0(in.dir,"tx_0.50deg_regular_1979-2018.nc"),
#' paste0(in.dir,"rh_0.50deg_regular_1979-2018.nc"),
#' paste0(in.dir,"rs_0.50deg_regular_1979-2018.nc"),
#' paste0(in.dir,"ws_0.50deg_regular_1979-2018.nc"))
#' author.data <- list(institution="Looney Bin", institution_id="LBC")
#' create.utci.from.files(input.files, out.file, input.files[1], author.data,  parallel=3)
#' }
#'
#' @export
create.utci.from.files <- function(input.files, output.file, author.data,  climdex.time.resolution="daily", axis.to.split.on="Y", parallel=4,
                                  verbose=FALSE, cluster.type="SOCK") {
  if(!(is.logical(parallel) || is.numeric(parallel)))
    stop("'parallel' option must be logical or numeric.")

  if(length(input.files) == 0)
    stop("Require at least one input file.")
  ## Load a json config file that contains the majority of the configurable options, e.g. long name, etc
  metadata.config = read_json_metadata_config_file()
  variable.name.map = metadata.config$get.variable.name.map()

  # Edit variable name map, only keep variables for utci calculation.
  variable.name.map = variable.name.map[c('tmin', 'tmax', 'rh', 'rs', 'ws')]

  f <- lapply(input.files, ncdf4::nc_open)
  f.meta <- create.file.metadata(f, variable.name.map)

  ## Create the output file
  utci.netcdf <- create.utci.file(output.file, f, f.meta$ts, f.meta$v.f.idx, variable.name.map, f.meta$dim.size, f.meta$dim.axes,
                                utci.dat, author.data)
  cluster <- set.up.cluster(parallel, cluster.type)
  max.vals.millions <- f.meta$dim.size[1] * f.meta$dim.size[3]
  subsets <- ncdf4.helpers::get.cluster.worker.subsets(max.vals.millions, f.meta$dim.size, f.meta$dim.axes, axis.to.split.on)

  write.utci.data <- function(out.list, out.sub) {
    dat <- out.list
    output <- t(matrix(unlist(dat),  ncol = f.meta$dim.size[1], byrow = FALSE))
    ncdf4.helpers::nc.put.var.subset.by.axes(utci.netcdf, "utci", output, out.sub)

    gc()
  }

  compute.utci.for.stripe <- function(subset, ts, dim.axes, v.f.idx, variable.name.map) {
    cat('We are now at latitude', subset[['Y']], '\n')
    # need to know what the latitude is.
    latitudes <- ncdf4.helpers::nc.get.dim.for.axis(utci.netcdf, "utci", "Y")
    longitudes <- ncdf4.helpers::nc.get.dim.for.axis(utci.netcdf, "utci", "X")
    lats <- latitudes[['vals']]
    lat <- lats[subset[['Y']]]
    lons <- longitudes[['vals']]

    data.list <- sapply(names(v.f.idx), function(x) { gc(); get.data(f[[v.f.idx[x]]], variable.name.map[x], subset, src.units[x], dim.axes) }, simplify=FALSE)
    gc()

    # Constuct input arguments for utci calculation function
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
    utci_input_arguments = c(potential_data, ts_arguments, "lons", "lat")
    utci_input_arguments$lat <- lat
    utci_input_arguments$lons <- lons

    r <- 1:(dim(data.list[[1]])[2])
    return(lapply(r, function(x) {
      concrete_args = utci_input_arguments
      concrete_args$tmin = concrete_args$tmin[,x]
      concrete_args$tmax = concrete_args$tmax[,x]
      concrete_args$rh = concrete_args$rh[,x]
      concrete_args$rs = concrete_args$rs[,x]
      concrete_args$ws = concrete_args$ws[,x]
      concrete_args$lons <- concrete_args$lons[[x]]
      return(UTCIr::calc_UTCI(concrete_args))
    }))
  }

  if(!is.null(cluster)) {
    lapply(f, ncdf4::nc_close)
    rm(f)
    snow::clusterExport(cluster, "input.files", environment())
    snow::clusterEvalQ(cluster, f <<- lapply(input.files, ncdf4::nc_open, readunlim=FALSE))

    ## Compute subsets and fire jobs off; collect and write out chunk-at-a-time
    parLapplyLBFiltered(cluster, subsets, compute.utci.for.stripe, f.meta$ts, f.meta$dim.axes, f.meta$v.f.idx, variable.name.map, local.filter.func=write.utci.data)

    snow::stopCluster(cluster)
  } else {

    lapply(subsets, function(x) { write.utci.data(compute.utci.for.stripe(x, f.meta$ts, f.meta$dim.axes, f.meta$v.f.idx, variable.name.map), x) })
    lapply(f, ncdf4::nc_close)
  }
  ## Close all the files
  ncdf4::nc_close(utci.netcdf)

  cat("Finished computing UTCI\n")
  invisible(0)
}



