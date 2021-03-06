% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create.thresholds.file.R
\name{create.thresholds.file}
\alias{create.thresholds.file}
\title{Creates Climdex thresholds output file.}
\usage{
create.thresholds.file(
  thresholds.file,
  f,
  ts,
  v.f.idx,
  variable.name.map,
  base.range,
  dim.size,
  dim.axes,
  threshold.dat,
  author.data
)
}
\arguments{
\item{thresholds.file}{The filename to be used for the thresholds file.}

\item{f}{The file(s) being used as sources for metadata.}

\item{ts}{The associated time data, as created by \code{nc.get.time.series}.}

\item{v.f.idx}{A mapping from variables to files, as created by \code{\link{get.var.file.idx}}.}

\item{variable.name.map}{A mapping from standardized names (tmax, tmin, prec) to NetCDF variable names.}

\item{base.range}{The base range; a vector of two numeric years.}

\item{dim.size}{Dimension sizes for the input.}

\item{dim.axes}{Dimension axes for the input.}

\item{threshold.dat}{Threshold metadata, as provided by \code{\link{get.thresholds.metadata}}.}

\item{author.data}{A vector containing named elements describing the author; see \code{\link{create.indices.from.files}}.}
}
\value{
An object of class \code{ncdf4}.
}
\description{
Creates Climdex thresholds output file.
}
\details{
This function creates a file suitable for outputting thresholds to, with all variables that can be created with the input data present in the file.
}
\examples{
\donttest{
## Establish basic inputs.
author.data <- list(institution="Looney Bin", institution_id="LBC")
input.files <- c("pr_NAM44_CanRCM4_ERAINT_r1i1p1_1989-2009.nc")

## Prepare derived inputs.
f <- lapply(input.files, ncdf4::nc_open)
variable.name.map <- c(tmax="tasmax", tmin="tasmin", prec="pr")
f.meta <- create.file.metadata(f, variable.name.map)
threshold.dat <- get.thresholds.metadata(names(f.meta$v.f.idx))

## Create output file
thresh.file <- create.thresholds.file("thresh.nc", f, f.meta$ts, f.meta$v.f.idx, variable.name.map,
                                      c(1981,1990), f.meta$dim.size, f.meta$dim.axes,
                                      threshold.dat, author.data)
}

}
