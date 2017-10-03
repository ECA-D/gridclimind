context('Do the metadata functions generate the correct metadata')

metadata.config = gridclimind:::read_json_metadata_config_file()

# Variables
input_data_path = system.file('extdata/example_ncfiles', package = 'gridclimind')
variable.name.map = metadata.config$get.variable.name.map()
threshold.name.map = metadata.config$get.thresholds.name.map()
climdex.time.resolution = 'all'
climdex.vars.subset = NULL   # Just use all you can compute
output.filename.template = 'rr_0.25deg_reg_1950-2016.nc'

# Open the NetCDF files
nc_file_list = list.files(input_data_path, pattern = '*2016.nc', full.names = TRUE)
open_nc_files = lapply(nc_file_list, ncdf4::nc_open)
threshold_files = list.files(input_data_path, pattern = '*_TH_*', full.names = TRUE)

# Get the various metadata and mappings
f.meta = create.file.metadata(open_nc_files, variable.name.map)
t.f.idx = get.thresholds.f.idx(threshold_files, threshold.name.map)
climdex.var.list = get.climdex.variable.list(names(f.meta$v.f.idx), metadata.config, climdex.time.resolution, climdex.vars.subset)
cdx.meta = get.climdex.variable.metadata(climdex.var.list, output.filename.template, metadata.config)
cdx.funcs = get.climdex.functions(climdex.var.list, metadata.config)
metadata = list(f.meta = f.meta, t.f.idx = t.f.idx, climdex.var.list = climdex.var.list, cdx.meta = cdx.meta, cdx.funcs = cdx.funcs)

test_that('The metadata and mappings have been performed correctly', {
  expect_equal_to_reference(metadata$f.meta, 'reference_rdsfiles/f.meta.rds')
  expect_equal_to_reference(metadata$t.f.idx, 'reference_rdsfiles/t.f.idx.rds')
  expect_equal_to_reference(metadata$climdex.var.list, 'reference_rdsfiles/climdex.var.list.rds')
  expect_equal_to_reference(metadata$cdx.meta, 'reference_rdsfiles/cdx.meta.rds')
  expect_equal_to_reference(metadata$cdx.funcs, 'reference_rdsfiles/cdx.funcs.rds')
})

## Change time resolution to 'month'
climdex.time.resolution = 'monthly'

f.meta = create.file.metadata(open_nc_files, variable.name.map)
t.f.idx = get.thresholds.f.idx(threshold_files, threshold.name.map)
climdex.var.list = get.climdex.variable.list(names(f.meta$v.f.idx), metadata.config, climdex.time.resolution, climdex.vars.subset)
cdx.meta = get.climdex.variable.metadata(climdex.var.list, output.filename.template, metadata.config)
cdx.funcs = get.climdex.functions(climdex.var.list, metadata.config)
metadata.monthly = list(f.meta = f.meta, t.f.idx = t.f.idx, climdex.var.list = climdex.var.list, cdx.meta = cdx.meta, cdx.funcs = cdx.funcs)

test_that('The metadata for monthly only is generated correctly', {
  expect_equal(metadata$f.meta, metadata.monthly$f.meta)
  expect_equal(metadata$t.f.idx, metadata.monthly$t.f.idx)
  expect_false(length(metadata$cdx.funcs) == length(metadata.monthly$cdx.funcs), info = 'switching to monthly should reduce the number of functions to compute')
  expect_true(all(c(length(metadata.monthly$climdex.var.list), length(metadata.monthly$cdx.funcs), nrow(metadata.monthly$cdx.meta)) == length(metadata.monthly$climdex.var.list)), info = 'the length of these objects should be equal')
  expect_equal_to_reference(metadata.monthly$climdex.var.list, 'reference_rdsfiles/climdex.var.list.monthly.rds')
  expect_equal_to_reference(metadata.monthly$cdx.funcs, 'reference_rdsfiles/cdx.funcs.monthly.rds')
  expect_equal_to_reference(metadata.monthly$cdx.meta, 'reference_rdsfiles/cdx.meta.monthly.rds')
})

## Subset to only a particular index
climdex.vars.subset = 'r75p'   # Just use all you can compute

f.meta = create.file.metadata(open_nc_files, variable.name.map)
t.f.idx = get.thresholds.f.idx(threshold_files, threshold.name.map)
climdex.var.list = get.climdex.variable.list(names(f.meta$v.f.idx), metadata.config, climdex.time.resolution, climdex.vars.subset)
cdx.meta = get.climdex.variable.metadata(climdex.var.list, output.filename.template, metadata.config)
cdx.funcs = get.climdex.functions(climdex.var.list, metadata.config)
metadata.r75p = list(f.meta = f.meta, t.f.idx = t.f.idx, climdex.var.list = climdex.var.list, cdx.meta = cdx.meta, cdx.funcs = cdx.funcs)

test_that('The metadata for r75p is generated correctly', {
  expect_equal(metadata$f.meta, metadata.r75p$f.meta)
  expect_equal(metadata$t.f.idx, metadata.r75p$t.f.idx)
  expect_equal(names(metadata.r75p$cdx.funcs), 'r75pETCCDI_mon')
  expect_false(length(metadata$cdx.funcs) == length(metadata.r75p$cdx.funcs), info = 'switching to r75p should reduce the number of functions to compute')
  expect_true(all(c(length(metadata.r75p$climdex.var.list), length(metadata.r75p$cdx.funcs), nrow(metadata.r75p$cdx.meta)) == length(metadata.r75p$climdex.var.list)), info = 'the length of these objects should be equal')
  expect_equal_to_reference(metadata.r75p$climdex.var.list, 'reference_rdsfiles/climdex.var.list.r75p.rds')
  expect_equal_to_reference(metadata.r75p$cdx.funcs, 'reference_rdsfiles/cdx.funcs.r75p.rds')
  expect_equal_to_reference(metadata.r75p$cdx.meta, 'reference_rdsfiles/cdx.meta.r75p.rds')
})

# Some cleanup
lapply(open_nc_files, ncdf4::nc_close)
