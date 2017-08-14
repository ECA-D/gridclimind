# This json configuration code is meant to replace all the hardcoded metadata


get.variable.metadata.from.json = function(json_metadata) {
  variable.metadata.rows = lapply(json_metadata$index.metadata, function(index_data) {
    if (length(index_data$supported.time.resolutions) > 1) {
      paste(json_metadata$generic.metadata$time.resolution.long.name.prefix[index_data$supported.time.resolutions], index_data$long.name, sep = ' ')
    } else {
      return(index_data$long.name)
    }
  })
  return(variable.metadata.rows)
}

read_json_metadata_config_file = function(json_path) {
  require(jsonlite)
  if (missing(json_path)) {
    if (!is.null(getOption('gridclimind_metadata_json_file'))) {
      json_path = getOption('gridclimind_metadata_json_file')
    } else {
      stop('You did not pass a valid `json_path`, nor is is the global option `gridclimind_metadata_json_file` set.')
    }
  }
  json_metadata = fromJSON(json_path)
  return(list(
    get.variable.metadata = function() get.variable.metadata.from.json(json_metadata)
  ))
}
# xi = read_json_metadata_config_file(system.file('extdata/metadata_config_files/eobs.json', package = 'gridclimind'))
# xi$get.variable.metadata()
