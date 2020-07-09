## Run package scPDSI to estimate scPDSI
#' Create scPDSI
#'
#' Create scPDSI
#'
#' The purpose of this function is to compute scPDSI on the data supplied, saving them to the file specified.
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
#' ## Prepare input data and calculate scPDSI for file.
#' input.files <- c(paste0(in.dir,"rr_0.50deg_regular_1971-2016.nc"),paste0(in.dir,"pet_0.50deg_regular_1971-2016.nc"))
#' author.data <- list(institution="Looney Bin", institution_id="LBC")
#' create.scPDSI.from.files(input.files, out.file, input.files[1], author.data,  parallel=3)
#' }
#'
#' @export
create.scPDSI.from.files <- function(input.files, output.file, author.data,  climdex.time.resolution="daily", axis.to.split.on="Y", parallel=4,
                                  verbose=FALSE, cluster.type="SOCK", start=NULL, end = NULL, cal_start=NULL, cal_end=NULL) {
  if(!(is.logical(parallel) || is.numeric(parallel)))
    stop("'parallel' option must be logical or numeric.")

  if(length(input.files) == 0)
    stop("Require at least one input file.")
  ## Load a json config file that contains the majority of the configurable options, e.g. long name, etc
  metadata.config = read_json_metadata_config_file()
  variable.name.map = metadata.config$get.variable.name.map()

  # Edit variable name map, only keep variables for scPDSI calculation.
  variable.name.map = variable.name.map[c('prec', 'pet')]

  f <- lapply(input.files, ncdf4::nc_open)
  f.meta <- create.file.metadata(f, variable.name.map)

  ## Create the output file
  scPDSI.netcdf <- create.scPDSI.file(output.file, f, f.meta$ts, f.meta$v.f.idx, variable.name.map, f.meta$dim.size, f.meta$dim.axes,
                                scPDSI.dat, author.data)
  cluster <- set.up.cluster(parallel, cluster.type)
  max.vals.millions <- f.meta$dim.size[1] * f.meta$dim.size[3]
  subsets <- ncdf4.helpers::get.cluster.worker.subsets(max.vals.millions, f.meta$dim.size, f.meta$dim.axes, axis.to.split.on)

  write.scPDSI.data <- function(out.list, out.sub) {
#    browser()
    dat <- out.list

#browser()
    output <- t(matrix(unlist(dat),  ncol = f.meta$dim.size[1], byrow = FALSE))
#    output[is.infinite(output)] <- NA
#    output <- apply(output, MARGIN=1, FUN=function(x) as.numeric(as.character(x)))
 #   output[output>1000] <- NA
    ncdf4.helpers::nc.put.var.subset.by.axes(scPDSI.netcdf, "scPDSI", output, out.sub)

    gc()
  }

  compute.scPDSI.for.stripe <- function(subset, ts, dim.axes, v.f.idx, variable.name.map) {
    cat('We are now at latitude', subset[['Y']], '\n')
    # need to know what the latitude is.
    latitudes <- ncdf4.helpers::nc.get.dim.for.axis(scPDSI.netcdf, "scPDSI", "Y")
    longitudes <- ncdf4.helpers::nc.get.dim.for.axis(scPDSI.netcdf, "scPDSI", "X")
    lats <- latitudes[['vals']]
    lat <- lats[subset[['Y']]]
    lons <- longitudes[['vals']]

    data.list <- sapply(names(v.f.idx), function(x) { gc(); get.data(f[[v.f.idx[x]]], variable.name.map[x], subset, src.units[x], dim.axes) }, simplify=FALSE)
    gc()

    # Constuct input arguments for scPDSI calculation function
    potential_data = lapply(names(variable.name.map), function(var) data.list[[var]])
    names(potential_data) = names(variable.name.map)
    ts_arguments = lapply(potential_data, function(x) {
      if (is.null(x)) {
        return(NULL)
      } else {
        return(ts)
      }
    })

 #   browser()

    names(ts_arguments) = paste(names(ts_arguments), 'dates', sep = '.')
    scPDSI_input_arguments = c(potential_data, ts_arguments, "lons", "lat")
    scPDSI_input_arguments$lat <- lat
    scPDSI_input_arguments$lons <- lons

    r <- 1:(dim(data.list[[1]])[2])
    return(lapply(r, function(x) {
      concrete_args = scPDSI_input_arguments
      concrete_args$prec = concrete_args$prec[,x]
      concrete_args$pet = concrete_args$pet[,x]
      concrete_args$lons <- concrete_args$lons[[x]]
      return(scPDSIr::pdsi(concrete_args, start = start, end = end, cal_start = cal_start, cal_end = cal_end))
    }))
  }

  if(!is.null(cluster)) {
    lapply(f, ncdf4::nc_close)
    rm(f)
    snow::clusterExport(cluster, "input.files", environment())
    snow::clusterEvalQ(cluster, f <<- lapply(input.files, ncdf4::nc_open, readunlim=FALSE))

    ## Compute subsets and fire jobs off; collect and write out chunk-at-a-time
    parLapplyLBFiltered(cluster, subsets, compute.scPDSI.for.stripe, f.meta$ts, f.meta$dim.axes, f.meta$v.f.idx, variable.name.map, local.filter.func=write.scPDSI.data)

    snow::stopCluster(cluster)
  } else {

    lapply(subsets, function(x) { write.scPDSI.data(compute.scPDSI.for.stripe(x, f.meta$ts, f.meta$dim.axes, f.meta$v.f.idx, variable.name.map), x) })
    lapply(f, ncdf4::nc_close)
  }
  ## Close all the files
  ncdf4::nc_close(scPDSI.netcdf)

  cat("Finished computing scPDSI\n")
  invisible(0)
}



