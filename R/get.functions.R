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
get.climdex.functions <- function(vars.list, metadata.config, fclimdex.compatible=TRUE) {
  func = metadata.config$get.functions(additional.arguments = list(rx5day = list(center.mean.on.last.day = fclimdex.compatible)))
  return(func[vars.list])
}
