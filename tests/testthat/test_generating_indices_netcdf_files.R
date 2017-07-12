context('Are index NetCDF files generated correctly')

input_data_path = system.file('extdata/example_ncfiles', package = 'gridclimind')
output_data_path = tempdir()
reference_file_path = 'reference_ncfiles'

author.data = list(Eobsv ="14.0", base.range="1981-2010")

# Remove old output files, code will not run otherwise
output_files_present = list.files(output_data_path, full.names = TRUE) %>% as.list()
if (length(output_files_present) > 0) do.call(file.remove, output_files_present)

# Generate rainfall indices
dummy = capture.output(create.indices.from.files(input.files = file.path(input_data_path, 'rr_0.25deg_reg_1950-2016.nc'),
                                                 thresholds.files = file.path(input_data_path, 'EOBS_TH_rr_v14.nc'),
                                                 out.dir = output_data_path,
                                                 author.data = author.data,
                                                 climdex.vars.subset = c('r75p', 'r95p', 'r99p',
                                                                         'r75ptot', 'r95ptot', 'r99ptot'),
                                                 output.filename.template = 'rr_0.25deg_reg_1950-2016.nc',
                                                 base.range=c(2017, 2021),
                                                 parallel=FALSE))

# Check that the files have been generated
rainfall_reference_files = list.files(reference_file_path, pattern = 'r*nc')
file_present_in_output = sapply(rainfall_reference_files, function(p) file.exists(file.path(output_data_path, p)))

## Check that the contents of the newly generated files equals that of the reference files
# TRUE means equal
ncdf_files_equal = function(file1, file2) {
  if (!file.exists(file1) | !file.exists(file2)) return(FALSE)
  cdo_available = system('cdo -h', ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
  if (!cdo_available) stop('Could not find cdo tools, please install...')
  diff_result = system(sprintf('cdo diff "%s" "%s"', file1, file2), intern = TRUE, ignore.stderr = TRUE)
  return(length(diff_result) == 0)
}

ncdf_files_equal_to_ref = sapply(rainfall_reference_files, function(p) {
  res = ncdf_files_equal(path.expand(file.path(output_data_path, p)),
                         path.expand(file.path(reference_file_path, p)))
})

# Perform all the checks
test_that('Rainfall index files where correctly generated', {
  expect_true(all(file_present_in_output), info = sprintf('Missing files %s.', paste(rainfall_reference_files[!file_present_in_output], collapse = ', ')))
  expect_true(all(ncdf_files_equal_to_ref), info = sprintf('Files differ from reference %s.', paste(rainfall_reference_files[!ncdf_files_equal_to_ref], collapse = ', ')))
})
