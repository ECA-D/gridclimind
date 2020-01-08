library(gridclimind)
author.data = list(Eobsv ="14.0", base.range="1981-2010")

output_file = 'EOBS_TH_TG_v14.nc'
file.remove(output_file)
create.thresholds.from.file(input.files = file.path(system.file('extdata/example_ncfiles', package = 'gridclimind'), 'tg_0.25deg_reg_1950-2016.nc'),
                            output.file = output_file,
                            author.data,
                            base.range=c(1991, 2000),
                            parallel=FALSE)

output_file = 'EOBS_TH_TX_TN_v14.nc'
file.remove(output_file)
create.thresholds.from.file(input.files = file.path(system.file('extdata/example_ncfiles', package = 'gridclimind'), c('tn_0.25deg_reg_1950-2016.nc', 'tx_0.25deg_reg_1950-2016.nc')),
                            output.file = output_file,
                            author.data,
                            base.range=c(1991, 2000),
                            parallel=FALSE)

output_file = 'EOBS_TH_rr_v14.nc'
file.remove(output_file)
create.thresholds.from.file(input.files = file.path(system.file('extdata/example_ncfiles', package = 'gridclimind'), c('rr_0.25deg_reg_1950-2016.nc')),
                            output.file = output_file,
                            author.data,
                            base.range=c(1991, 2000),
                            parallel=FALSE)
