context('Are index NetCDF files generated correctly')
# When running the tests manually, execute the following line first
# library(gridclimind);setwd('tests/testthat/')
source('support_functions.R')

author.data = list(Eobsv ="14.0", base.range="1981-2010")

# Remove old output files, code will not run otherwise
delete_all_content_in_temp_path()

# Generate rainfall indices
input_files = paste(input_data_path, c('rr_0.25deg_reg_1950-2016.nc', 'tn_0.25deg_reg_1950-2016.nc', 'tx_0.25deg_reg_1950-2016.nc', 'tg_0.25deg_reg_1950-2016.nc'), sep = '/')
input_th_files = paste(input_data_path, c('EOBS_TH_rr_v14.nc', 'EOBS_TH_TX_TN_v14.nc', 'EOBS_TH_TG_v14.nc'), sep = '/')
dummy = suppressMessages(capture.output(create.indices.from.files(input.files = input_files,
                                                 thresholds.files = input_th_files,
                                                 out.dir = output_data_path,
                                                 author.data = author.data,
                                                 climdex.vars.subset = NULL, # Calculate any that the package can calculate based on the input data
                                                 output.filename.template = 'rr_0.25deg_reg_1950-2016.nc',
                                                 base.range=c(2017, 2021),
                                                 parallel=FALSE)))

# Check that the files have been generated
current_reference_path = file.path(reference_file_path, 'generate_all_indices_test')
reference_files = list.files(current_reference_path)
file_present_in_output = sapply(reference_files, function(p) file.exists(file.path(output_data_path, p)))

## Check that the contents of the newly generated files equals that of the reference files
# TRUE means equal
ncdf_files_equal_to_ref = sapply(reference_files, function(p) {
  res = ncdf_files_equal(path.expand(file.path(output_data_path, p)),
                         path.expand(file.path(current_reference_path, p)))
})
ncdf_metadata_equal_to_ref = sapply(reference_files, function(p) {
  res = ncdf_files_metadata_equal(path.expand(file.path(output_data_path, p)),
                                  path.expand(file.path(current_reference_path, p)))
})

# Perform all the checks
test_that('Index files where correctly generated', {
  expect_true(all(file_present_in_output), info = sprintf('Missing files %s.', paste(reference_files[!file_present_in_output], collapse = ', ')))
  expect_true(all(ncdf_files_equal_to_ref), info = sprintf('Files differ from reference %s.', paste(reference_files[!ncdf_files_equal_to_ref], collapse = ', ')))
  expect_true(all(ncdf_metadata_equal_to_ref), info = sprintf('Metadata differs from reference %s.', paste(reference_files[!ncdf_metadata_equal_to_ref], collapse = ', ')))
})
