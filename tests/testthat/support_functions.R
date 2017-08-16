input_data_path = system.file('extdata/example_ncfiles', package = 'gridclimind')
output_data_path = tempdir()
reference_file_path = 'reference_ncfiles'

ncdf_files_equal = function(file1, file2) {
  if (!file.exists(file1) | !file.exists(file2)) return(FALSE)
  cdo_available = system('cdo -h', ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
  if (!cdo_available) stop('Could not find cdo tools, please install...')
  diff_result = system(sprintf('cdo diff "%s" "%s"', file1, file2), intern = TRUE, ignore.stderr = TRUE)
  return(length(diff_result) == 0)
}

delete_all_content_in_temp_path = function() {
  if (!any(startsWith(output_data_path, c('/var', '/tmp')))) stop(sprintf('The output_data_path as a suspicious start, please check: %s', output_data_path))
  output_files_present = list.files(output_data_path, full.names = TRUE) %>% as.list()
  if (length(output_files_present) > 0) do.call(unlink, c(list(output_files_present), list(recursive = TRUE)))
  return(invisible(NULL))
}
