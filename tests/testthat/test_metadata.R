context('Do the metadata functions generate the correct metadata')

# Variables
input_data_path = system.file('extdata/example_ncfiles', package = 'gridclimind')
variable.name.map = c(tmax="tx", tmin="tn", prec="rr", tavg="tg")
threshold.name.map = c(tx10thresh="tx10thresh", tn10thresh="tn10thresh",
  tx90thresh="tx90thresh", tn90thresh="tn90thresh",
  r75thresh="r75thresh", r95thresh="r95thresh", r99thresh="r99thresh")
climdex.time.resolution = 'all'
climdex.vars.subset = NULL   # Just use all you can compute
output.filename.template = 'rr_0.25deg_reg_1950-2016.nc'

# Open the NetCDF files
nc_file_list = list.files(input_data_path, pattern = '*2016.nc', full.names = TRUE)
open_nc_files = lapply(nc_file_list, ncdf4::nc_open)
threshold_files = list.files(input_data_path, pattern = '*_TH_*', full.names = TRUE)

# Get the various metadata and mappings
f.meta = create.file.metadata(open_nc_files, variable.name.map)
t.f.idx <- get.thresholds.f.idx(threshold_files, threshold.name.map)
climdex.var.list <- get.climdex.variable.list(names(f.meta$v.f.idx), climdex.time.resolution, climdex.vars.subset)
cdx.meta <- get.climdex.variable.metadata(climdex.var.list, output.filename.template)
cdx.funcs <- get.climdex.functions(climdex.var.list)

# Some cleanup
lapply(open_nc_files, ncdf4::nc_close)

test_that('The metadata and mappings have been performed correctly', {
  expect_equal_to_reference(f.meta, 'reference_rdsfiles/f.meta.rds')
  expect_equal_to_reference(t.f.idx, 'reference_rdsfiles/t.f.idx.rds')
  expect_equal_to_reference(climdex.var.list, 'reference_rdsfiles/climdex.var.list.rds')
  expect_equal_to_reference(cdx.meta, 'reference_rdsfiles/cdx.meta.rds')
  expect_equal_to_reference(cdx.funcs, 'reference_rdsfiles/cdx.funcs.rds')
})
