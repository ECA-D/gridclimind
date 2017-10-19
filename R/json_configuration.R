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
               time.res = index_data$supported.time.resolutions,
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

get.indices.for.which.data.is.present.from.json = function(source.data.present, json_metadata) {
  index_data = json_metadata$index.metadata
  required.variables = lapply(index_data, '[[', 'required.variables')
  all_data_present_for_index = sapply(required.variables, function(x) all(x %in% source.data.present))
  return(names(index_data)[all_data_present_for_index])
}

get.functions.from.json = function(json_metadata, additional.arguments) {
  index_data = json_metadata$index.metadata
  all_functions = lapply(names(json_metadata$index.metadata), function(index.id) {
    index_data = json_metadata$index.metadata[[index.id]]
    actual_function = getFromNamespace(index_data$calculation.function.name, 'climind')
    curried_functions = sapply(index_data$supported.time.resolutions, function(time.res) {
      args = list(actual_function, freq = time.res)
      if (!is.null(index_data$additional_arguments)) args = c(args, index_data$additional_arguments)
      if (index.id %in% names(additional.arguments)) args = modifyList(args, additional.arguments[[index.id]]) # Add or replace any of the manually passed arguments
      do.call(functional:::Curry, args)
    })
    names(curried_functions) = paste(index.id, json_metadata$generic.metadata$index.category, '_', json_metadata$generic.metadata$time.resolution.postfix[index_data$supported.time.resolutions], sep = '')
    return(curried_functions)
  })
  return(unlist(all_functions))
}

get.thresholds.metadata.from.json = function(json_metadata) {
  return(json_metadata$generic.metadata$threshold.metadata)
}

get.variable.name.map.from.json = function(json_metadata) {
  variable.name.map = as.character(json_metadata$generic.metadata$variable.name.map)
  names(variable.name.map) = names(json_metadata$generic.metadata$variable.name.map)
  return(variable.name.map)
}

get.thresholds.name.map.from.json = function(json_metadata) {
  thold.metadata = json_metadata$generic.metadata$threshold.metadata
  thresholds.name.map = sapply(thold.metadata, '[[', 'name')
  names(thresholds.name.map) = names(thold.metadata)
  return(thresholds.name.map)
}

get.time.resolution.postfix.from.json = function(json_metadata) {
  return(json_metadata$generic.metadata$time.resolution.postfix)
}

get.threshold.path.from.json = function(json_metadata) {
  thold_metadata = json_metadata$generic.metadata$threshold.metadata
  all_paths = lapply(thold_metadata, '[[', 'q.path')
  path_length = sapply(all_paths, length)
  stopifnot(all(path_length %in% c(3,2)))
  return(list('1d' = all_paths[path_length == 2], '2d' = all_paths[path_length == 3]))
}

get.variables.requiring.quantiles.from.json = function(index_names, json_metadata) {
  postfix_removed = sapply(strsplit(index_names, split = '_'), '[', 1)
  remove_index_category = sub(json_metadata$generic.metadata$index.category, '', postfix_removed)
  unique_indices = unique(remove_index_category)
  indices_requiring_quantiles = sapply(json_metadata$index.metadata[unique_indices], '[[', 'needs.quantiles')
  indices_requiring_quantiles = names(indices_requiring_quantiles[indices_requiring_quantiles])
  if (length(indices_requiring_quantiles) == 0) return(character())   # No indices that are needed require quantiles, return empty string
  variables_requiring_quantiles = sapply(json_metadata$index.metadata[indices_requiring_quantiles], '[[', 'required.variables')
  return(unique(unlist(variables_requiring_quantiles)))
}

# This function reads the json file matching the global setting 'metadata.id', and expects a matching file to exist
# in 'extdata/metadata_config_files/'. To add more json files, simply copy one of the existing files and edit the
# information. To change metadata settings, simply edit the appropriate json file and rebuild the package. People using
# that new package will use the new metadata settings.
#
# The json files contain two types of metadata:
#
# - Generic metadata, e.g. how the variables tmin and tmax are called in the input NCDF files.
# - Index related metadata, for example how to name the variables in the output NCDF files.
read_json_metadata_config_file = function(json_path) {
  require(jsonlite)
  if (missing(json_path)) {
    if (!is.null(getOption('metadata.id'))) {
      metadata.id = getOption('metadata.id')
    } else {
      metadata.id = 'eobs'
    }
    json_path = system.file(sprintf('extdata/metadata_config_files/%s.json', tolower(metadata.id)), package = 'gridclimind')
  }
  json_metadata = fromJSON(json_path)
  return(list(
    get.variable.metadata = function() get.variable.metadata.from.json(json_metadata),
    get.variable.list = function(index.ids, time.resolution) get.variable.list.from.json(index.ids, time.resolution, json_metadata),
    get.indices.for.which.data.is.present = function(source.data.present) get.indices.for.which.data.is.present.from.json(source.data.present, json_metadata),
    get.functions = function(additional.arguments) get.functions.from.json(json_metadata, additional.arguments),
    get.thresholds.metadata = function() get.thresholds.metadata.from.json(json_metadata),
    get.variable.name.map = function() get.variable.name.map.from.json(json_metadata),
    get.thresholds.name.map = function() get.thresholds.name.map.from.json(json_metadata),
    get.time.resolution.postfix = function() get.time.resolution.postfix.from.json(json_metadata),
    get.threshold.path = function() get.threshold.path.from.json(json_metadata),
    get.variables.requiring.quantiles = function(index_names) get.variables.requiring.quantiles.from.json(index_names, json_metadata)
  ))
}
