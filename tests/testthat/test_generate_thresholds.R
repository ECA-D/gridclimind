context('Are threshold NetCDF files correctly generated')
source('support_functions.R')

author.data <- list(institution="Looney Bin", institution_id="LBC", Eobsv ="14.0", base.range="1981-2010")

# Remove old output files, code will not run otherwise
delete_all_content_in_temp_path()

output_file = file.path(output_data_path, 'TH_test.nc')
dummy = capture.output(create.thresholds.from.file(input.files = file.path(input_data_path, 'rr_0.25deg_reg_1950-2016.nc'),
                            output.file = output_file,
                            author.data,
                            base.range=c(1991, 2000),
                            parallel=FALSE))

test_that('the threshold files have been generated correctly', {
  expect_true(file.exists(output_file), info = 'can the output file be found')
  expect_true(ncdf_files_equal(output_file, file.path(reference_file_path, 'TH_test.nc')))
})
