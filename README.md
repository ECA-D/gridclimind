[![Gitter](https://img.shields.io/gitter/room/nwjs/nw.js.svg)](https://gitter.im/eca-d/climate_indices)
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) 

[![Travis-CI Build Status](https://travis-ci.org/ECA-D/gridclimind.svg?branch=master)](https://travis-ci.org/ECA-D/gridclimind) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/ECA-D/gridclimind?branch=master&svg=true)](https://ci.appveyor.com/project/ECA-D/gridclimind) [![Coverage Status](https://img.shields.io/codecov/c/github/ECA-D/gridclimind/master.svg)](https://codecov.io/github/ECA-D/gridclimind?branch=master)

# gridclimind
climate indices tool for E-OBS data (used only with ecad_indices_stations)

# Common tasks
## Add new index

For an example of how to add indices from `climind` into `gridclimind` you can have a look at the 7d185d65 commit in `gridclimind` (‘Added last three compound indices…etc’). In general, adding an index you will have to take the following steps:

1. Add a function to climind which uses the correct interface, i.e.:

        function(ci, freq=c("annual”), cur_sub)

    Where:
    
    - `ci` is the input object, this will be provided by `gridlcimind`.
    - `freq` the supported list of time resolutions. Typically this will be `freq=c("monthly", "annual", "halfyear", "seasonal”)`.
    - additional arguments needed. Note that in general these can only be static arguments either having a default value or a static value defined in the json file.

2. Add the metadata information to eobs.json (`inst/extdata/metadata_config_files` in the gridclimind project) in the `index.metadata` list. For example:

        "csdi": {
          "long.name": "Cold Spell Duration Index",
          "units": "days",
          "base.period.attr": true,
          "standard.name": "cold_spell_duration_index",
          "calculation.function.name": "climdex.csdi",
          "additional_arguments": {
            "spells.can.span.years": false
          },
          "supported.time.resolutions": "annual",
          "include.time.prefix.in.long.name": true,
          "required.variables": "tmin"
        }

    Note that:

    - `additional_arguments` is optional. These extra input arguments are fed to the listed `calculation.function.name`, i.e. the new function we created in step 1.
    - `supported.time.resolutions` and `required.variables` can also be a list of values, e.g.:

            "ww": {
              "long.name": "Warm-wet days",
              "units": "",
              "base.period.attr": false,
              "standard.name": "warm_wet_days",
              "calculation.function.name": "climdex.wd",
              "additional_arguments": {
                "precip.thresh": "q75",
                "precip.op": ">",
                "temp.thresh": "q75",
                "temp.op": ">"
              },
              "supported.time.resolutions": ["annual", "monthly", "seasonal", "halfyear"],
              "include.time.prefix.in.long.name": true,
              "required.variables": ["prec", "tavg"]
            }
    
3. Add the index file to the test suite:

    1. Run the test manually.
    2. Go to the output directory, this is listed in the `output_data_path` variable, e.g.:

            output_data_path
            [1] "/var/folders/dm/smk5_rf53bg057ql34rmhnq80000gn/T//RtmpphG0Nw"

        Copy the nc file for the index you added to `/your/installation/of/gridclimind/tests/testthat/reference_ncfiles/generate_all_indices_test/`. This could be up to 4 files, depended on the supported time resolutions.

    3. Commit the changes in the gridlcimind package to git.
