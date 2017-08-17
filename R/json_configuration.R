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

get.variable.list.from.json = function(index.ids, time.resolution, json_metadata) {
  relevant_index_data = json_metadata$index.metadata[index.ids]
  dat = sapply(names(relevant_index_data), function(index.id) {
    index_data = relevant_index_data[[index.id]]
    relevant_postfixes = json_metadata$generic.metadata$time.resolution.postfix[index_data$supported.time.resolutions]
    if (time.resolution != 'all') {
      relevant_postfixes = relevant_postfixes[time.resolution]
      if (is.null(relevant_postfixes[[1]])) return(NULL)   # For this particular index, the asked time.resolution is not supported
    }
    return(paste(index.id, json_metadata$generic.metadata$index.category, '_', relevant_postfixes, sep = ''))
  })
  dat = dat[!sapply(dat, is.null)]   # Remove any empty indices, i.e. no matching supported time.resolution
  names(dat) = NULL
  result = unlist(dat)
  if (is.null(result)) stop('No matching variables where found')
  return(result)
}

get.src.data.required.from.json = function(json_metadata) {
  source_data_per_index = lapply(json_metadata$index.metadata, '[[', 'required.variables')
  if (any(sapply(source_data_per_index, length) > 1)) stop('Cannot yet deal with indices that require more than one source variable')
  possible_source_data = unique(unlist(source_data_per_index))
  required_data_per_index = lapply(possible_source_data, function(src) {
    names(source_data_per_index)[source_data_per_index == src]
  })
  names(required_data_per_index) = possible_source_data
  return(required_data_per_index)
}

get.functions.from.json = function(json_metadata) {
  index_data = json_metadata$index.metadata
  all_functions = lapply(names(json_metadata$index.metadata), function(index.id) {
    index_data = json_metadata$index.metadata[[index.id]]
    actual_function = getFromNamespace(index_data$calculation.function.name, 'climind')
    curried_functions = sapply(index_data$supported.time.resolutions, function(time.res) {
      args = list(actual_function, freq = time.res)
      if (!is.null(index_data$additional_arguments)) args = c(args, index_data$additional_arguments)
      do.call(functional:::Curry, args)
    })
    names(curried_functions) = paste(index.id, json_metadata$generic.metadata$index.category, '_', json_metadata$generic.metadata$time.resolution.postfix[index_data$supported.time.resolutions], sep = '')
    return(curried_functions)
  })
  return(unlist(all_functions))
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
    get.variable.list = function(index.ids, time.resolution) get.variable.list.from.json(index.ids, time.resolution, json_metadata),
    get.src.data.required = function() get.src.data.required.from.json(json_metadata),
    get.functions = function() get.functions.from.json(json_metadata)
  ))
}
# xi = read_json_metadata_config_file(system.file('extdata/metadata_config_files/eobs.json', package = 'gridclimind'))
# xi$get.variable.metadata()
