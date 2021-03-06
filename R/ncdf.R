#' gridclimind, a package to calculate Climdex indices from NetCDF files.
#'
#' This package implements code to facilitate computation of Climdex indices
#' from NetCDF input files.
#'
#' The Climdex climate extremes indices have historically been calculated using
#' Fortran code. This has a number of problems:\itemize{
#' \item{Difficult to test}
#' \item{Difficult to modify (for instance, to add NetCDF file I/O)}
#' \item{Difficult to parallelize}
#' }
#' The \code{climind} package provides an easy interface to efficient
#' computation of Climdex indices. This package is complementary to it, providing
#' easy access to functions to compute indices in parallel, using NetCDF files as
#' input and output. It implements chunked processing of input files to keep memory
#' usage reasonable; it implements parallel computation using the \code{snow}
#' library; and it includes a test suite to verify correctness of the implementation.
#' Furthermore, the package has a modular design, allowing for easy extension to
#' allow for adaptation to changing or custom requirements. For example, the metadata
#' is stored in json files that are included with the package. This allows the core
#' of the code to separate from the metadata details, for example allowing one to switch
#' on-the-fly.
#'
#' Users of this package should pay particular attention to the
#' \code{\link{create.indices.from.files}} function, which computes Climdex indices
#' given NetCDF input files; and \code{\link{create.thresholds.from.file}}, which
#' computes thresholds for use with threshold-based indices given NetCDF input files.
#' Many of the other functions exposed by the package are intended to provide for
#' extensibility, but are unlikely to be routinely used by users of this package.
#'
#' @name gridclimind
#' @aliases gridclimind-package
#' @docType package
#' @seealso \code{\link{create.indices.from.files}}, \code{\link{create.thresholds.from.file}}
#' @references \url{http://etccdi.pacificclimate.org/list_27_indices.shtml}
#'
#' Karl, T.R., N. Nicholls, and A. Ghazi, 1999: CLIVAR/GCOS/WMO workshop on
#' indices and indicators for climate extremes: Workshop summary. Climatic
#' Change, 42, 3-7.
#'
#' Peterson, T.C., and Coauthors: Report on the Activities of the Working Group
#' on Climate Change Detection and Related Rapporteurs 1998-2001. WMO, Rep.
#' WCDMP-47, WMO-TD 1071, Geneve, Switzerland, 143pp.
#'
#' Zhang, X., 2005: Avoiding inhomogeneity in percentile-based indices of
#' temperature extremes. Journal of Climate 18.11 (2005):1641-.
#' @keywords climate ts
#' @import snow PCICt
NULL
