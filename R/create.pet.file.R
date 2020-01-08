#' Creates Climdex pet output file.
#'
#' Creates Climdex pet output file.
#'
#' This function creates a file suitable for outputting pet to.
#'
#' @param pet.file The filename to be used for the pet file.
#' @param f The file(s) being used as sources for metadata.
#' @param ts The associated time data, as created by \code{nc.get.time.series}.
#' @param v.f.idx A mapping from variables to files, as created by \code{\link{get.var.file.idx}}.
#' @param variable.name.map A mapping from standardized names (tmax, tmin, etc) to NetCDF variable names.
#' @param dim.size Dimension sizes for the input.
#' @param dim.axes Dimension axes for the input.
#' @param pet.dat pet metadata
#' @param author.data A vector containing named elements describing the author; see \code{\link{create.indices.from.files}}.
#' @return An object of class \code{ncdf4}.
#'
#'
#' @export
create.pet.file <- function(pet.file, f, ts, v.f.idx, variable.name.map, dim.size, dim.axes, pet.dat, author.data) {

  exemplar.file <- f[[v.f.idx[1]]]
  exemplar.var.name <- variable.name.map[names(v.f.idx)[1]]
  exemplar.var <- exemplar.file$var[[exemplar.var.name]]

  ## Get time metadata...
  old.time.dim <- exemplar.var$dim[[which(dim.axes == "T")]]
  time.units <- old.time.dim$units
  time.units.split <- strsplit(time.units, " ")[[1]]
  time.origin <- if(time.units.split[2] == "as") format(trunc(min(ts), units="days"), "%Y-%m-%d") else time.units.split[3]
  time.dim.name <- old.time.dim$name

  ## Set up time variables
  out.time <- exemplar.var$dim[[3]]$vals

  ####################### create output netcdf file #########################
  # Get x and y vectors (dimensions)
  lon_vals <- exemplar.var$dim[[1]]$vals
  lat_vals <- exemplar.var$dim[[2]]$vals
  lon_name <- exemplar.var$dim[[1]]$name
  lat_name <- exemplar.var$dim[[2]]$name
  lon_units <- exemplar.var$dim[[1]]$units
  lat_units <- exemplar.var$dim[[2]]$units
  cal <- exemplar.var$dim[[3]]$calendar

  # Define the dimensions
  dimX = ncdf4::ncdim_def(name=lon_name, units=lon_units, vals=lon_vals)
  dimY = ncdf4::ncdim_def(name=lat_name, units=lat_units, vals=lat_vals)
  dimT = ncdf4::ncdim_def(name=time.dim.name, units=time.units, vals=(out.time), calendar=cal, longname=time.dim.name)

  # Define missing value
  mv = -9999

  # Define the data
  out.var = ncdf4::ncvar_def( "pet", units="mm/day", list(dimX,dimY,dimT), mv, prec="float", longname = "Potential Evapotranspiration")

  ############################################################################
  ## Create file
  pet.netcdf <- ncdf4::nc_create(pet.file, out.var, force_v4=TRUE)

  return(pet.netcdf)
}
