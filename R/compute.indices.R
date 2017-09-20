## Compute all indices for a single grid box
#' Compute Climdex indices using provided data.
#'
#' Compute Climdex indices using provided data.
#'
#' Given the provided data and functions, compute the Climdex indices defined by the functions.
#'
#' @param in.dat The input data to compute indices on.
#' @param cdx.funcs The functions to be applied to the data, as created by \code{\link{get.climdex.functions}}.
#' @param ts The associated time data, as created by \code{nc.get.time.series}.
#' @param base.range The base range; a vector of two numeric years.
#' @return A list of data for each index.
#'
#' @examples
#' library(climind)
#'
#' ## Prepare input data
#' in.dat <- list(tmax=ec.1018935.tmax$MAX_TEMP)
#' cdx.funcs <- get.climdex.functions(get.climdex.variable.list(names(in.dat)))
#' in.dat$northern.hemisphere <- TRUE
#' ts <- as.PCICt(do.call(paste, ec.1018935.tmax[,c("year", "jday")]),
#'                format="%Y %j", cal="gregorian")
#'
#' ## Compute indices
#' res <- compute.climdex.indices(in.dat, cdx.funcs, ts, c(1981, 1990), FALSE)
#'
#' @export
compute.climdex.indices <- function(in.dat, cdx.funcs, ts, base.range) {

  ci <- climind::climdexInput.raw(
    tmax = in.dat$tmax, tmin = in.dat$tmin, prec = in.dat$prec,  tavg=in.dat$tavg,
    tmax.dates = if(is.null(in.dat$tmax)) NULL else ts,
    tmin.dates = if(is.null(in.dat$tmin)) NULL else ts,
    prec.dates = if(is.null(in.dat$prec)) NULL else ts,
    tavg.dates = if(is.null(in.dat$tavg)) NULL else ts,
    base.range=base.range, northern.hemisphere=in.dat$northern.hemisphere,
    quantiles=in.dat$quantiles)

  ## NOTE: Names must be stripped here because it increases memory usage on the head by a factor of 8-9x (!)
  return(lapply(cdx.funcs, function(f) { d <- f(ci=ci); names(d) <- NULL; d }))
}
