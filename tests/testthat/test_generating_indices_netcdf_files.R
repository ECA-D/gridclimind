context('Are index NetCDF files generated correctly')
source('support_functions.R')

author.data = list(Eobsv ="14.0", base.range="1981-2010")

# Remove old output files, code will not run otherwise
delete_all_content_in_temp_path()

# Generate rainfall indices
dummy = suppressMessages(capture.output(create.indices.from.files(input.files = file.path(input_data_path, 'rr_0.25deg_reg_1950-2016.nc'),
                                                 thresholds.files = file.path(input_data_path, 'EOBS_TH_rr_v14.nc'),
                                                 out.dir = output_data_path,
                                                 author.data = author.data,
                                                 climdex.vars.subset = c('r75p', 'r95p', 'r99p',
                                                                         'r75ptot', 'r95ptot', 'r99ptot'),
                                                 output.filename.template = 'rr_0.25deg_reg_1950-2016.nc',
                                                 base.range=c(2017, 2021),
                                                 parallel=FALSE)))

# Check that the files have been generated
rainfall_reference_files = list.files(reference_file_path, pattern = '^r')
file_present_in_output = sapply(rainfall_reference_files, function(p) file.exists(file.path(output_data_path, p)))

## Check that the contents of the newly generated files equals that of the reference files
# TRUE means equal
ncdf_files_equal_to_ref = sapply(rainfall_reference_files, function(p) {
  res = ncdf_files_equal(path.expand(file.path(output_data_path, p)),
                         path.expand(file.path(reference_file_path, p)))
})
ncdf_metadata_equal_to_ref = sapply(rainfall_reference_files, function(p) {
  res = ncdf_files_metadata_equal(path.expand(file.path(output_data_path, p)),
                                  path.expand(file.path(reference_file_path, p)))
})

# Perform all the checks
test_that('Rainfall index files where correctly generated', {
  expect_true(all(file_present_in_output), info = sprintf('Missing files %s.', paste(rainfall_reference_files[!file_present_in_output], collapse = ', ')))
  expect_true(all(ncdf_files_equal_to_ref), info = sprintf('Files differ from reference %s.', paste(rainfall_reference_files[!ncdf_files_equal_to_ref], collapse = ', ')))
  expect_true(all(ncdf_metadata_equal_to_ref), info = sprintf('Metadata differs from reference %s.', paste(rainfall_reference_files[!ncdf_metadata_equal_to_ref], collapse = ', ')))
})
