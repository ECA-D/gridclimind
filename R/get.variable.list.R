#' Returns a list of Climdex variables given constraints
#'
#' Returns a list of Climdex variables given constraints.
#'
#' This function takes a character vector which specifies what source data is present and a time resolution, and generates a list of names consisting of the variable and the time resolution, separated by an underscore.
#'
#' @param source.data.present A vector of strings naming the data that's present; at least one of (tmin, tmax, prec, tavg).
#' @param time.resolution The time resolutions to compute indices at. See \code{\link{create.indices.from.files}}.
#' @param climdex.vars.subset A character vector of lower-case names of Climdex indices to calculate (eg: tr, fd, rx5day). See \code{\link{create.indices.from.files}}.
#' @return A character vector containing variable names with time resolutions appended.
#'
#' @seealso \code{\link{create.indices.from.files}}
#' @examples
#' ## Get all variables which require tmin and/or tmax, for all time resolutions.
#' var.list1 <- get.climdex.variable.list(c("tmax", "tmin"))
#'
#' ## Get all variables which require prec with an annual time resolution.
#' var.list2 <- get.climdex.variable.list("prec", time.resolution="annual")
#'
#' ## Get the intersection of a set list of vars and available data.
#' sub.vars <- c("su", "id", "tr", "fd", "gsl", "csdi", "wsdi", "r10mm")
#' var.list3 <- get.climdex.variable.list("tmax", climdex.vars.subset=sub.vars)
#'
#' @export
get.climdex.variable.list <- function(source.data.present, time.resolution=c("all", "annual", "monthly"), climdex.vars.subset=NULL) {
  time.res <- match.arg(time.resolution)
  annual.only <- c("hiETCCDI","gslETCCDI",  "cddETCCDI", "cwdETCCDI",  "altcddETCCDI", "altcwdETCCDI", "csdiETCCDI",
                   "altcsdiETCCDI", "wsdiETCCDI", "altwsdiETCCDI")
  monthly.only <- c("spi3ETCCDI","spi6ETCCDI")
  vars.by.src.data.reqd <- list(tmax=c("csuETCCDI", "suETCCDI", "idETCCDI", "txxETCCDI", "txnETCCDI", "tx10pETCCDI", "tx90pETCCDI", "wsdiETCCDI", "altwsdiETCCDI"),
                                tmin=c("cfdETCCDI", "fdETCCDI", "trETCCDI", "tnxETCCDI", "tnnETCCDI", "tn10pETCCDI", "tn90pETCCDI", "csdiETCCDI", "altcsdiETCCDI"),
                                prec=c("spi3ETCCDI", "spi6ETCCDI", "rx1dayETCCDI", "rx5dayETCCDI", "sdiiETCCDI", "r10mmETCCDI", "r20mmETCCDI", "r1mmETCCDI",
                                       "cddETCCDI", "cwdETCCDI", "r75pETCCDI", "r95pETCCDI", "r99pETCCDI", "r75ptotETCCDI", "r95ptotETCCDI", "r99ptotETCCDI",
                                       "prcptotETCCDI", "altcddETCCDI", "altcwdETCCDI"),
                                tavg=c("hd17ETCCDI", "hiETCCDI", "gslETCCDI", "dtrETCCDI") )

  if(any(!(source.data.present %in% c("tmin", "tmax", "tavg", "prec"))))
    stop("Invalid variable listed in source.data.present.")

  if(all(c("tmax", "tmin") %in% source.data.present) && !("tavg" %in% source.data.present))
    source.data.present <- c(source.data.present, "tavg")

  climdex.vars <- unlist(vars.by.src.data.reqd[source.data.present])
  if(!is.null(climdex.vars.subset))
    climdex.vars <- climdex.vars[climdex.vars %in% paste(climdex.vars.subset, "ETCCDI", sep="")]

  #freq.lists <- list(c("mon", "yr"), c("yr"))
  freq.lists <- list(c("mon", "yr"), c("yr"),c("mon"))

  helper_fun <- function(climdex.vars, annual.only, month.only) {
    if (climdex.vars %in% annual.only) {
      return(paste(climdex.vars, 'yr', sep = '_'))
    } else if (climdex.vars %in% monthly.only) {
      return(paste(climdex.vars, 'mon', sep = '_'))
    } else {
      return(paste(climdex.vars, c('mon', 'yr'), sep = '_'))
    }
  }

  #   dat <- switch(time.res,
  #                 all=unlist(lapply(climdex.vars, function(x) { paste(x, freq.lists[[(x %in% annual.only) + 1]], sep="_") })),
  #                 annual=paste(climdex.vars, "yr", sep="_"),
  #                 monthly=paste(climdex.vars[!(climdex.vars %in% annual.only)], "mon", sep="_"))

  dat <- switch(time.res,
                all=unlist(lapply(climdex.vars, helper_fun, annual.only = annual.only, month.only = month.only)),
                annual=paste(climdex.vars[!(climdex.vars %in% monthly.only)], "yr", sep="_"),
                monthly=paste(climdex.vars[!(climdex.vars %in% annual.only)], "mon", sep="_"))

  names(dat) <- NULL

  return(dat)
}
