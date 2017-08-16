# This json configuration code is meant to replace all the hardcoded metadata


get.variable.metadata.from.json = function(json_metadata) {
  variable.metadata.rows = lapply(names(json_metadata$index.metadata), function(index_id) {
    index_data = json_metadata$index.metadata[[index_id]]
    time.res.ln.prefix = json_metadata$generic.metadata$time.resolution.long.name.prefix
    index.time.postfix = json_metadata$generic.metadata$time.resolution.postfix
    if (index_data$include.time.prefix.in.long.name) {
      long.name = paste(time.res.ln.prefix[index_data$supported.time.resolutions], index_data$long.name, sep = ' ')
    } else {
      long.name = index_data$long.name
    }
    data.frame(long.name = long.name,
               var.name = paste(index_id, json_metadata$generic.metadata$index.category, sep = ''),
               units = index_data$units,
               annual = index_data$supported.time.resolutions == 'annual',
               base.period.attr = index_data$base.period.attr,
               row.names = paste(index_id, json_metadata$generic.metadata$index.category, '_', index.time.postfix[index_data$supported.time.resolutions], sep = ''),
               standard.name = index_data$standard.name,
               stringsAsFactors = FALSE)
  })
  return(do.call('rbind', variable.metadata.rows))
}
get.index.variable = function(index_id, variable_id, json_metadata) {
  index_metadata = json_metadata$index.metadata
  if (!index_id %in% names(index_metadata)) stop(sprintf('Unkown index %s', index_id))
  if (!variable_id %in% names(index_metadata[[index_id]])) stop(sprintf('Unknown variable %s for index %s', variable_id, index_id))
  index_metadata[[index_id]][[variable_id]]
}

read_json_metadata_config_file = function(json_path) {
  require(jsonlite)
  if (missing(json_path)) {
    if (!is.null(getOption('metadata.id'))) {
      metadata.id = getOption('metadata.id')
    } else {
      metadata.id = 'eobs'
    }
    json_path = system.file(sprintf('extdata/metadata_config_files/%s.json', metadata.id), package = 'gridclimind')
  }
  json_metadata = fromJSON(json_path)
  return(list(
    get.variable.metadata = function() get.variable.metadata.from.json(json_metadata),
    get.index.variable = function(index_id, variable_id) get.index.variable(index_id, variable_id, json_metadata)
  ))
}
# xi = read_json_metadata_config_file(system.file('extdata/metadata_config_files/eobs.json', package = 'gridclimind'))
# xi$get.variable.metadata()
