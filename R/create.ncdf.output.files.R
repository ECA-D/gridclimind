#' Creates output files for Climdex variables.
#'
#' Creates output files for Climdex variables.
#'
#' This function creates a set of output files for the set of variable parameters passed in \code{cdx.dat}, as created by \code{\link{get.climdex.variable.metadata}}. It copies metadata from input files as appropriate and adds new metadata as required.
#'
#' @param cdx.dat The variable description data, as created by \code{\link{get.climdex.variable.metadata}}.
#' @param f The file(s) being used as input.
#' @param v.f.idx A mapping from variables to files, as created by \code{\link{get.var.file.idx}}.
#' @param variable.name.map A mapping from standardized names (tmax, tmin, prec) to NetCDF variable names.
#' @param ts The associated time data, as created by \code{nc.get.time.series}.
#' @param time.origin The time origin, as specified in the source NetCDF file(s).
#' @param base.range The base range; a vector of two numeric years.
#' @param out.dir The output directory name.
#' @param author.data A vector containing named elements describing the author; see \code{\link{create.indices.from.files}}.
#' @return A list of objects of type \code{ncdf4}.
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
#' climdex.var.list <- get.climdex.variable.list(names(f.meta$v.f.idx), "all", NULL)
#' cdx.meta <- get.climdex.variable.metadata(climdex.var.list, input.files[1])
#'
#' ## Create output files
#' cdx.ncfile <- create.ncdf.output.files(cdx.meta, f, f.meta$v.f.idx, variable.name.map,
#'                                        f.meta$ts, get.time.origin(f, f.meta$dim.axes),
#'                                        c(1981,1990), "/foo", author.data)
#' }
#'
#' @export
create.ncdf.output.files <- function(cdx.dat, f, v.f.idx, variable.name.map, ts, time.origin, base.range, out.dir, author.data, metadata.config) {
  file_postfixes = metadata.config$get.time.resolution.postfix()

  f.example <- f[[v.f.idx[1]]]
  v.example <- variable.name.map[names(v.f.idx)[1]]
  time.dim.name <- ncdf4.helpers::nc.get.dim.for.axis(f.example, v.example, "T")$name
  old.time.bnds.att <- ncdf4::ncatt_get(f.example, time.dim.name, "bounds")
  time.bnds.name <- if(old.time.bnds.att$hasatt) old.time.bnds.att$value else paste(time.dim.name, "bnds", sep="_")

  ## Create new time dimensions
  time.origin.PCICt <- PCICt::as.PCICt.default(time.origin, cal=attr(ts, "cal"))
  time.units <- paste("days since", time.origin)

  input.bounds <- ncdf4.helpers::nc.get.dim.bounds.var.list(f.example, v.example)
  ## FIXME: I'm not sure how solid the assumption about the location of bnds here is.
  bnds <- if(length(input.bounds) > 0) f.example$var[[input.bounds[1]]]$dim[[1]] else ncdf4::ncdim_def("bnds", "", 1:2, create_dimvar=FALSE)
  time.dat <- list(annual    = get.output.time.data(ts, time.origin.PCICt, time.units, time.dim.name, time.bnds.name, bnds, res="year"),
                   monthly   = get.output.time.data(ts, time.origin.PCICt, time.units, time.dim.name, time.bnds.name, bnds, res="month"),
                   seasonal  = get.output.time.data(ts, time.origin.PCICt, time.units, time.dim.name, time.bnds.name, bnds, res="season"),
                   halfyear  = get.output.time.data(ts, time.origin.PCICt, time.units, time.dim.name, time.bnds.name, bnds, res="halfyear"))

  grid.mapping.att <- ncdf4::ncatt_get(f.example, v.example, "grid_mapping")
  vars.to.copy <- c(input.bounds[input.bounds != time.bnds.name], names(ncdf4.helpers::nc.get.coordinate.axes(f.example, v.example)), if(grid.mapping.att$hasatt) grid.mapping.att$value)
  vars.to.clone.atts.for <- c(vars.to.copy, ncdf4.helpers::nc.get.dim.names(f.example, v.example))
  vars.ncvars <- sapply(vars.to.copy, function(x) { f.example$var[[x]] }, simplify=FALSE)
  vars.data <- lapply(vars.ncvars, function(ncvar) { if(length(ncvar$dim) == 0) NULL else ncdf4::ncvar_get(f.example, ncvar) })

  return(lapply(1:length(cdx.dat$var.name), function(x) {
    time.for.file <- time.dat[[cdx.dat$time.res[x]]]

    ## Establish variables, create file
    nc.var.list <- c(vars.ncvars, list(time.for.file$time.bnds.var, ncdf4::ncvar_def(name=cdx.dat$var.name[x], units=cdx.dat$units[x], dim=c(f.example$var[[v.example]]$dim[1:2], list(time.for.file$time.dim)), missval=1e20, longname=cdx.dat$long.name[x])))
    new.file <- ncdf4::nc_create(paste(out.dir, cdx.dat$filename[x], sep="/"), nc.var.list, force_v4=TRUE)

    ## Copy attributes for all variables plus global attributes
    att.rename <- c("frequency"="input_frequency", "creation_date"="input_creation_date", "title"="input_title", "tracking_id"="input_tracking_id")
    inst.id <- ncdf4::ncatt_get(f.example, 0, "institution_id")
    if(inst.id$hasatt) {
      att.rename.inst <- c("contact"="contact", "references"="references")
      names(att.rename.inst) <- paste(inst.id$value, names(att.rename.inst), sep="_")
      att.rename <- c(att.rename, att.rename.inst)
    }

    ## Copy attributes with renaming and exclusions.
    ncdf4.helpers::nc.copy.atts(f.example, 0, new.file, 0, definemode=TRUE, rename.mapping=att.rename)
    ncdf4.helpers::nc.copy.atts(f.example, v.example, new.file, cdx.dat$var.name[x], definemode=TRUE, exception.list=c("units", "long_name", "standard_name", "base_period", "missing_value", "_FillValue", "add_", "valid_min", "valid_max", "valid_range", "scale_factor", "add_offset", "signedness", "history"))
    for(v in vars.to.clone.atts.for) {
      ncdf4.helpers::nc.copy.atts(f.example, v, new.file, v, definemode=TRUE)
    }
    ncdf4::ncatt_put(new.file, time.dim.name, "units", time.units, definemode=TRUE)

    ## Put additional attributes.
    put.history.att(new.file, cdx.dat$var.name[x], definemode=TRUE)
    put.ETCCDI.atts(new.file, file_postfixes[[cdx.dat$time.res[x]]], ncdf4::ncatt_get(f.example, 0, "title")$value, author.data, definemode=TRUE)
    if(cdx.dat$base.period.attr[x])
      ncdf4::ncatt_put(new.file, cdx.dat$var.name[x], "base_period", paste(author.data$base.range, sep=""), definemode=TRUE)
    ncdf4::nc_enddef(new.file)

    ## Copy data from vars.to.copy and put time bounds.
    ncdf4::ncvar_put(new.file, time.bnds.name, time.for.file$time.bnds.data)
    for(v in vars.to.copy)
      if(!is.null(vars.data[[v]]))
         ncdf4::ncvar_put(new.file, v, vars.data[[v]])

    new.file
  }))
}
