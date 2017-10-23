context('Are index NetCDF files generated correctly')
# When running the tests manually, execute the following line first
# library(gridclimind);setwd('tests/testthat/')
source('support_functions.R')

author.data = list(Eobsv ="14.0", base.range="1981-2010")

# Remove old output files, code will not run otherwise
delete_all_content_in_temp_path()

# Generate rainfall indices
input_files = paste(input_data_path, c('rr_0.25deg_reg_1950-2016.nc', 'tn_0.25deg_reg_1950-2016.nc', 'tx_0.25deg_reg_1950-2016.nc', 'tg_0.25deg_reg_1950-2016.nc', 'cc_0.25deg_reg_1950-2016.nc'), sep = '/')
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

# Check for files that have been generated in the test, but have no reference.
output_files = list.files(output_data_path, pattern = '*nc')
files_with_no_reference = setdiff(output_files, reference_files)

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
  expect_true(length(files_with_no_reference) == 0, info = sprintf('Could not find reference NetCDF files for the following generated index files: %s', paste(files_with_no_reference, collapse = ', ')))
})

# Remove old output files, code will not run otherwise
delete_all_content_in_temp_path()

## Test running the code when no quantiles are passed
## In this case this should work as the fd index does not require quantiles
# Remove old output files, code will not run otherwise
delete_all_content_in_temp_path()
dummy = suppressMessages(capture.output(create.indices.from.files(input.files = input_files,
                                                 thresholds.files = NULL,
                                                 out.dir = output_data_path,
                                                 author.data = author.data,
                                                 climdex.vars.subset = 'fd',
                                                 output.filename.template = 'rr_0.25deg_reg_1950-2016.nc',
                                                 base.range=c(2017, 2021),
                                                 parallel=FALSE)))

fd_index_files = list.files(output_data_path, full.names = TRUE)
fd_reference_files = file.path(current_reference_path, basename(fd_index_files))
fd_ref_files_exist = sapply(fd_reference_files, file.exists)
fd_ncdf_header_the_same = sapply(fd_index_files, function(fname) {
  ncdf_files_metadata_equal(fname, file.path(current_reference_path, basename(fname)))
})
fd_ncdf_data_the_same = sapply(fd_index_files, function(fname) {
  ncdf_files_equal(fname, file.path(current_reference_path, basename(fname)))
})

test_that('Quantiles can be ignored if they are not needed for the indices that will be calculated', {
  expect_true(all(fd_ref_files_exist))
  expect_true(all(fd_ncdf_header_the_same))
  expect_true(all(fd_ncdf_data_the_same))
})

## Now run a test that requires indices. Also note that the base period is inside the
## date range of the data. This is a special case as this rquires inbase quantiles. These
## cannot be precalculated, so calculating them on-the-fly is the only way to solve the issues.
# Remove old output files, code will not run otherwise
delete_all_content_in_temp_path()
dummy = suppressMessages(capture.output(create.indices.from.files(input.files = input_files,
                                                                  thresholds.files = NULL,
                                                                  out.dir = output_data_path,
                                                                  author.data = author.data,
                                                                  climdex.vars.subset = 'tn10p',
                                                                  output.filename.template = 'rr_0.25deg_reg_1950-2016.nc',
                                                                  base.range=c(1960, 1990),
                                                                  parallel=FALSE)))

tn10p_index_files = list.files(output_data_path, full.names = TRUE)
tn10p_reference_files = file.path(current_reference_path, basename(tn10p_index_files))
tn10p_ref_files_exist = sapply(tn10p_reference_files, file.exists)
tn10p_ncdf_header_the_same = sapply(tn10p_index_files, function(fname) {
  ncdf_files_metadata_equal(fname, file.path(current_reference_path, basename(fname)))
})
tn10p_ncdf_data_the_same = sapply(tn10p_index_files, function(fname) {
  ncdf_files_equal(fname, file.path(current_reference_path, basename(fname)))
})

test_that('Quantiles can be ignored if they are not needed for the indices that will be calculated', {
  expect_true(all(tn10p_ref_files_exist))
  expect_true(all(tn10p_ncdf_header_the_same))
  expect_true(all(!tn10p_ncdf_data_the_same))    # The reference was calculated with only outbase quantiles, so the result should be different now.
})
