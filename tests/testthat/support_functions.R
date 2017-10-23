input_data_path = system.file('extdata/example_ncfiles', package = 'gridclimind')
output_data_path = tempdir()
reference_file_path = 'reference_ncfiles'

check_for_nccmp = function() {
  nccmp_available = system('nccmp -h', ignore.stdout = TRUE, ignore.stderr = TRUE) == 2
  if (!nccmp_available) stop('Could not find nccmp, please install...(http://nccmp.sourceforge.net/)')
}

# http://nccmp.sourceforge.net/
ncdf_files_equal = function(file1, file2) {
  if (!file.exists(file1) | !file.exists(file2)) return(FALSE)
  check_for_nccmp()
  diff_result = suppressWarnings(system2('nccmp', c('-dNf', file1, file2), stdout = TRUE, stderr = TRUE))
  return(length(diff_result) == 0)
}

ncdf_files_metadata_equal = function(file1, file2) {
  if (!file.exists(file1) | !file.exists(file2)) return(FALSE)
  check_for_nccmp()
  diff_result = suppressWarnings(system2('nccmp', c('-mf', file1, file2), stdout = TRUE, stderr = TRUE))
  time_related_metadata = which(grepl('ATTRIBUTE : history', diff_result))
  cat(diff_result[-time_related_metadata])
  return(length(diff_result[-time_related_metadata]) == 0)
}

delete_all_content_in_temp_path = function() {
  if (!any(startsWith(output_data_path, c('/var', '/tmp')))) stop(sprintf('The output_data_path as a suspicious start, please check: %s', output_data_path))
  output_files_present = as.list(list.files(output_data_path, full.names = TRUE))
  if (length(output_files_present) > 0) do.call(unlink, c(list(output_files_present), list(recursive = TRUE)))
  return(invisible(NULL))
}
