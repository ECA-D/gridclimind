% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/support_functions.R
\name{create.climdex.eobs.filenames}
\alias{create.climdex.eobs.filenames}
\title{Creates a list of CMIP5-compliant filenames reflecting the input data.}
\usage{
create.climdex.eobs.filenames(fn.split, vars.list)
}
\arguments{
\item{fn.split}{A vector containing named components, as created by \code{get.split.filename.cmip5}.}

\item{vars.list}{A vector containing names of variables, as created by \code{\link{get.climdex.variable.list}}.}
}
\value{
A vector containing filenames corresponding to the variables and filename bits supplied.
}
\description{
Creates a list of CMIP5-compliant filenames reflecting the input data.
}
\details{
This function takes a split filename (as created by \code{get.split.filename.cmip5}) and a list of variables and creates corresponding filenames for the given variables.
}
\examples{
\dontrun{
library(ncdf4.helpers)
## Split out filename bits for use below...
fn <- "pr_day_BCCAQ+ANUSPLIN300+MRI-CGCM3_historical+rcp85_r1i1p1_19500101-21001231.nc"
fn.split <- get.split.filename.cmip5(fn)

## Create filenames with time data and variable appropriately replaced.
filenames <- create.climdex.cmip5.filenames(fn.split, c("rx5dayETCCDI_mon", "tn90pETCCDI_yr"))
}

create.climdex.cmip5.filenames <- function(fn.split, vars.list) {
time.res <- c("yr", "mon")[grepl("_mon$", vars.list) + 1]
time.range <- substr(fn.split[c('tstart', 'tend')], 1, 4)

paste(paste(vars.list, fn.split['model'], fn.split['emissions'], fn.split['run'], sapply(time.res, function(x) { paste(time.range, switch(x, yr=c("", ""), mon=c("01", "12")), sep="", collapse="-") }), sep="_"), ".nc", sep="")
}
}
