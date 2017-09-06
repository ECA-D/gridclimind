#' Returns metadata for specified Climdex variables
#'
#' Returns metadata for specified Climdex variables.
#'
#' This function returns metadata suitable for use in NetCDF files for the specified variables.
#'
#' @param vars.list The list of variables, as returned by \code{\link{get.climdex.variable.list}}.
#' @param template.filename The filename template to be used when generating filenames.
#' @param metadata.config config object read using \code{read_json_metadata_config_file}. This contains all the metadata such as the output long names of the indices in the output NCDF files.
#' @return A data frame containing the following:
#' \itemize{
#' \item{long.name}{Long names for the variable}
#' \item{var.name}{Variable name for use in the file}
#' \item{units}{Units for the variable}
#' \item{annual}{Whether the variable is annual}
#' \item{base.period.attr}{Whether to include a base period attribute}
#' \item{standard.name}{Standard name to use for the variable}
#' \item{filename}{Filename to be written out}
#' }
#'
#' @examples
#' ## Get metadata (including filenames) for specified variables.
#' fn <- "pr_day_BCCAQ+ANUSPLIN300+MRI-CGCM3_historical+rcp85_r1i1p1_19500101-21001231.nc"
#' var.list2 <- get.climdex.variable.list("prec", time.resolution="annual")
#' md <- get.climdex.variable.metadata(var.list2, fn)
#'
#' @export
get.climdex.variable.metadata <- function(vars.list, template.filename, metadata.config) {
  all.data = metadata.config$get.variable.metadata()

  all.data$filename <- create.climdex.eobs.filenames(get.split.filename.eobs(template.filename), rownames(all.data))
  return(all.data[vars.list,])
}
