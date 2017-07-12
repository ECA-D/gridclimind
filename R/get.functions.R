#' Returns a list of Climdex functions, with parameters curried in.
#'
#' Returns a list of Climdex functions, with parameters curried in.
#'
#' This function takes a variable list (as created by \code{\link{get.climdex.variable.list}}) and creates a list of functions corresponding to the specified indices, with parameters such as time resolution curried in. This allows for these functions to be called with just the \code{t} object as an argument, easing the automation of computing indices.
#'
#' @param vars.list The variable list, as created by \code{\link{get.climdex.variable.list}}.
#' @param fclimdex.compatible Whether to create fclimdex compatible functions.
#' @return A list of functions, named by the variable they compute.
#'
#' @examples
#' ## Get Climdex functions for a variable list with all appropriate params
#' ## curried in, so that all they take is a t object.
#' cdx.funcs <- get.climdex.functions(get.climdex.variable.list(c("tmax", "tmin")))
#'
#' @export
get.climdex.functions <- function(vars.list, fclimdex.compatible=TRUE) {
  func.names <- c("climdex.fd", "climdex.su", "climdex.id", "climdex.tr", "climdex.gsl",
                  "climdex.fd", "climdex.su", "climdex.id", "climdex.tr",

                  "climdex.txx", "climdex.tnx", "climdex.txn", "climdex.tnn", "climdex.tn10p", "climdex.tx10p", "climdex.tn90p", "climdex.tx90p",
                  "climdex.txx", "climdex.tnx", "climdex.txn", "climdex.tnn", "climdex.tn10p", "climdex.tx10p", "climdex.tn90p", "climdex.tx90p",

                  "climdex.wsdi", "climdex.csdi", "climdex.wsdi", "climdex.csdi",

                  "climdex.dtr", "climdex.rx1day", "climdex.rx5day",
                  "climdex.dtr", "climdex.rx1day", "climdex.rx5day",

                  "climdex.sdii", "climdex.r10mm", "climdex.r20mm", "climdex.rnnmm", "climdex.cdd", "climdex.cwd", "climdex.r95ptot", "climdex.r99ptot", "climdex.prcptot",
                  "climdex.sdii", "climdex.r10mm", "climdex.r20mm", "climdex.rnnmm", "climdex.r95ptot", "climdex.r99ptot", "climdex.prcptot",

                  "climdex.cdd", "climdex.cwd")
  #
  func.names <- c(func.names, "climdex.r75p", "climdex.r75p", "climdex.r95p", "climdex.r95p", "climdex.r99p", "climdex.r99p",
                  "climdex.r75ptot", "climdex.r75ptot", "climdex.csu", "climdex.csu", "climdex.cfd", "climdex.cfd",
                  "climdex.hd17", "climdex.hd17", "climdex.HI", "climdex.spi3", "climdex.spi6")


  el <- list()
  af <- list(freq="annual")
  mf <- list(freq="monthly")
  cwdd.opts <- list(spells.can.span.years=TRUE)
  altcwdd.opts <- list(spells.can.span.years=FALSE)
  wcsdi.opts <- list(spells.can.span.years=FALSE)
  altwcsdi.opts <- list(spells.can.span.years=TRUE)
  rx5day.opts <- list(center.mean.on.last.day=fclimdex.compatible)
  r1mm.opts <- list(threshold=1)
  options <- list( af, af, af, af, el,
                   mf, mf, mf, mf,

                   mf, mf, mf, mf, mf, mf, mf, mf,
                   af, af, af, af, af, af, af, af,

                   c(af, wcsdi.opts), c(af, wcsdi.opts), c(af, altwcsdi.opts), c(af, altwcsdi.opts),

                   mf, mf, c(mf, rx5day.opts),
                   af, af, c(af, rx5day.opts),

                   af, af, af, c(af, r1mm.opts), c(af,cwdd.opts), c(af,cwdd.opts), af, af, af,
                   mf, mf, mf, c(mf, r1mm.opts), mf, mf, mf,
                   c(af, altcwdd.opts), c(af, altcwdd.opts))


  options <- c(options, list(af, mf, af, mf, af, mf,
                             af, mf, af, mf, af, mf,
                             af, mf, af, mf, mf))

  func <- lapply(1:length(func.names), function(n) do.call(functional::Curry, c(list(getFromNamespace(func.names[n], 'climind')), options[[n]])))

  names(func) <- c("fdETCCDI_yr", "suETCCDI_yr", "idETCCDI_yr","trETCCDI_yr", "gslETCCDI_yr",
                   "fdETCCDI_mon", "suETCCDI_mon", "idETCCDI_mon","trETCCDI_mon",

                   "txxETCCDI_mon", "tnxETCCDI_mon", "txnETCCDI_mon", "tnnETCCDI_mon", "tn10pETCCDI_mon", "tx10pETCCDI_mon", "tn90pETCCDI_mon", "tx90pETCCDI_mon",
                   "txxETCCDI_yr",  "tnxETCCDI_yr",  "txnETCCDI_yr",  "tnnETCCDI_yr",  "tn10pETCCDI_yr",  "tx10pETCCDI_yr",  "tn90pETCCDI_yr",  "tx90pETCCDI_yr",

                   "wsdiETCCDI_yr", "csdiETCCDI_yr",  "altwsdiETCCDI_yr", "altcsdiETCCDI_yr",

                   "dtrETCCDI_mon", "rx1dayETCCDI_mon", "rx5dayETCCDI_mon",
                   "dtrETCCDI_yr", "rx1dayETCCDI_yr", "rx5dayETCCDI_yr",

                   "sdiiETCCDI_yr", "r10mmETCCDI_yr", "r20mmETCCDI_yr", "r1mmETCCDI_yr", "cddETCCDI_yr", "cwdETCCDI_yr", "r95ptotETCCDI_yr", "r99ptotETCCDI_yr", "prcptotETCCDI_yr",
                   "sdiiETCCDI_mon", "r10mmETCCDI_mon", "r20mmETCCDI_mon", "r1mmETCCDI_mon", "r95ptotETCCDI_mon", "r99ptotETCCDI_mon", "prcptotETCCDI_mon",

                   "altcddETCCDI_yr", "altcwdETCCDI_yr",

                   "r75pETCCDI_yr", "r75pETCCDI_mon", "r95pETCCDI_yr", "r95pETCCDI_mon", "r99pETCCDI_yr", "r99pETCCDI_mon",
                   "r75ptotETCCDI_yr", "r75ptotETCCDI_mon", "csuETCCDI_yr", "csuETCCDI_mon", "cdfETCCDI_yr", "cdfETCCDI_mon",
                   "hd17ETCCDI_yr", "hd17ETCCDI_mon", "hiETCCDI_yr", "spi3ETCCDI_mon", "spi6ETCCDI_mon")

  return(func[vars.list])
}
