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
  var_names = names(in.dat)[!names(in.dat) %in% c('northern.hemisphere', 'quantiles')]
  stopifnot(length(setdiff(names(in.dat), c(c('northern.hemisphere', 'quantiles'), var_names))) == 0)   # Check that there are no names in in.dat that we do not expect

  # Construct list of variables and their associated date information and make ci object
  raw_input_args = lapply(var_names, function(v) {
    ret = list(in.dat[[v]], ts)
    names(ret) = c(v, paste(v, 'dates', sep = '.'))
    return(ret)
  })
  ci = do.call(climind::climdexInput.raw, c(unlist(raw_input_args, recursive = FALSE),
                                            list(base.range=base.range,
                                                 northern.hemisphere=in.dat$northern.hemisphere,
                                                 quantiles=in.dat$quantiles)))

  ## NOTE: Names must be stripped here because it increases memory usage on the head by a factor of 8-9x (!)
  return(lapply(cdx.funcs, function(f) { d <- f(ci=ci); names(d) <- NULL; d }))
}
