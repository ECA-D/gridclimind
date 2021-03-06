% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create.thresholds.from.file.R
\name{create.thresholds.from.file}
\alias{create.thresholds.from.file}
\title{Create Climdex thresholds used for computing threshold-based indices}
\usage{
create.thresholds.from.file(
  input.files,
  output.file,
  author.data,
  axis.to.split.on = "Y",
  base.range = c(1961, 1990),
  parallel = 4,
  verbose = FALSE,
  max.vals.millions,
  cluster.type = "SOCK"
)
}
\arguments{
\item{input.files}{A list of filenames of NetCDF files to be used as input. A NetCDF file may contain one or more variables.}

\item{output.file}{The name of the file to be created.}

\item{author.data}{A vector containing named elements describing the author; see \code{\link{create.indices.from.files}}.}

\item{axis.to.split.on}{The axis to split up the data on for parallel / incremental processing.}

\item{base.range}{Vector of two numeric years specifying the start and end years.}

\item{parallel}{The number of parallel processing threads, or FALSE if no parallel processing is desired.}

\item{verbose}{Whether to be chatty.}

\item{max.vals.millions}{The number of data values to process at one time (length of time dim * number of values * number of variables).}

\item{cluster.type}{The cluster type, as used by the \code{snow} library.}
}
\description{
Create Climdex thresholds used for computing threshold-based indices
}
\details{
For many applications, one may want to compute thresholds on one data set, then apply them to another. This is usually the case when comparing GCM (Global Climate Model)
results for future time periods to either historical reanalysis data or historical / pre-industrial control runs from models.
The purpose of this function is to compute these thresholds on the data supplied, saving them to the file specified.
Then these thresholds can be used with \code{\link{create.indices.from.files}} to compute indices using the thresholds computed using this code.

The metadata is stored in JSON files that are included with the pacakge. Right now, the metadata relevant to EOBS is used by default. To switch to another set of metadata, use the \code{metadata.id}
global option:

    \code{options(metadata.id = 'eobs')}

Note that currently only EOBS metadata is available (\code{metadata.id = 'eobs'}).
}
\note{
NetCDF input files may contain one or more variables, named as per \code{variable.name.map} (read from config json file). The code will search the files for the named variables.
}
\examples{
\dontrun{
## Prepare input data and calculate thresholds for file.
input.files <- c("pr_NAM44_CanRCM4_ERAINT_r1i1p1_1989-2009.nc")
author.data <- list(institution="Looney Bin", institution_id="LBC")
create.thresholds.from.file(input.files, "thresh.nc", author.data,
                            base.range=c(1991, 2000), parallel=FALSE)
}

}
