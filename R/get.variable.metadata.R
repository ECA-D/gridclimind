#' Returns metadata for specified Climdex variables
#'
#' Returns metadata for specified Climdex variables.
#'
#' This function returns metadata suitable for use in NetCDF files for the specified variables.
#'
#' @param vars.list The list of variables, as returned by \code{\link{get.climdex.variable.list}}.
#' @param template.filename The filename template to be used when generating filenames.
#' @return A data frame containing the following:
#' \itemize{
#' \item{long.name}{Long names for the variable}
#' \item{var.name}{Variable name for use in the file}
#' \item{units}{Units for the variable}
#' \item{annual}{Whether the variable is annual}
#' \item{base.period.attr}{Whether to include a base period attribute}
#' \item{standard.name}{Standard name to use for the variable}
#' \item{filename}{Filename to be written out}
#' }
#'
#' @examples
#' ## Get metadata (including filenames) for specified variables.
#' fn <- "pr_day_BCCAQ+ANUSPLIN300+MRI-CGCM3_historical+rcp85_r1i1p1_19500101-21001231.nc"
#' var.list2 <- get.climdex.variable.list("prec", time.resolution="annual")
#' md <- get.climdex.variable.metadata(var.list2, fn)
#'
#' @export
get.climdex.variable.metadata <- function(vars.list, template.filename) {
  all.data <- data.frame(long.name=c("Annual Number of Frost Days", "Annual Number of Summer Days", "Annual Number of Icing Days", "Annual Number of Tropical Nights", "Growing Season Length",
                                     "Monthly Number of Frost Days", "Monthly Number of Summer Days", "Monthly Number of Icing Days", "Monthly Number of Tropical Nights",

                                     "Monthly Maximum of Daily Maximum Temperature", "Monthly Maximum of Daily Minimum Temperature",
                                     "Monthly Minimum of Daily Maximum Temperature", "Monthly Minimum of Daily Minimum Temperature",

                                     "Percentage of Days when Daily Minimum Temperature is Below the 10th Percentile", "Percentage of Days when Daily Maximum Temperature is Below the 10th Percentile",
                                     "Percentage of Days when Daily Minimum Temperature is Above the 90th Percentile", "Percentage of Days when Daily Maximum Temperature is Above the 90th Percentile",
                                     "Annual Maximum of Daily Maximum Temperature", "Annual Maximum of Daily Minimum Temperature",
                                     "Annual Minimum of Daily Maximum Temperature", "Annual Minimum of Daily Minimum Temperature",
                                     "Percentage of Days when Daily Minimum Temperature is Below the 10th Percentile", "Percentage of Days when Daily Maximum Temperature is Below the 10th Percentile",
                                     "Percentage of Days when Daily Minimum Temperature is Above the 90th Percentile", "Percentage of Days when Daily Maximum Temperature is Above the 90th Percentile",

                                     "Annnual Warm Spell Duration Index", "Annual Cold Spell Duration Index", "Annual Warm Spell Duration Index Spanning Years", "Annual Cold Spell Duration Index Spanning Years",

                                     "Mean Diurnal Temperature Range", "Monthly Maximum 1-day Precipitation", "Monthly Maximum Consecutive 5-day Precipitation",
                                     "Mean Diurnal Temperature Range", "Annual Maximum 1-day Precipitation", "Annual Maximum Consecutive 5-day Precipitation",

                                     "Annual Simple Precipitation Intensity Index", "Annual Count of Days with At Least 10mm of Precipitation",
                                     "Annual Count of Days with At Least 20mm of Precipitation", "Annual Count of Days with At Least 1mm of Precipitation",
                                     "Maximum Number of Consecutive Days with Less Than 1mm of Precipitation", "Maximum Number of Consecutive Days with At Least 1mm of Precipitation",
                                     "Annual Precipitation fraction due to very wet days (daily precipitation exceeds 95th percentile)",
                                     "Annual Precipitation fraction due to extremely wet days (daily precipitation exceeds 99th percentile)", "Annual Total Precipitation in Wet Days",

                                     "Monthly Simple Precipitation Intensity Index", "Monthly Count of Days with At Least 10mm of Precipitation",
                                     "Monthly Count of Days with At Least 20mm of Precipitation", "Monthly Count of Days with At Least 1mm of Precipitation",
                                     "Monthly Precipitation fraction due to very wet days (daily precipitation exceeds 95th percentile)",
                                     "Monthly Precipitation fraction due to extremely wet days (daily precipitation exceeds 99th percentile)", "Monthly Total Precipitation in Wet Days",

                                     "Maximum Number of Consecutive Days Per Year with Less Than 1mm of Precipitation", "Maximum Number of Consecutive Days Per Year with At Least 1mm of Precipitation"),

                         var.name=c("fdETCCDI", "suETCCDI","idETCCDI", "trETCCDI", "gslETCCDI",
                                    "fdETCCDI", "suETCCDI","idETCCDI", "trETCCDI",

                                    "txxETCCDI", "tnxETCCDI", "txnETCCDI", "tnnETCCDI", "tn10pETCCDI", "tx10pETCCDI", "tn90pETCCDI", "tx90pETCCDI",
                                    "txxETCCDI", "tnxETCCDI", "txnETCCDI", "tnnETCCDI", "tn10pETCCDI", "tx10pETCCDI", "tn90pETCCDI", "tx90pETCCDI",

                                    "wsdiETCCDI", "csdiETCCDI", "altwsdiETCCDI", "altcsdiETCCDI",

                                    "dtrETCCDI", "rx1dayETCCDI", "rx5dayETCCDI",
                                    "dtrETCCDI", "rx1dayETCCDI", "rx5dayETCCDI",

                                    "sdiiETCCDI", "r10mmETCCDI", "r20mmETCCDI", "r1mmETCCDI", "cddETCCDI", "cwdETCCDI",  "r95ptotETCCDI", "r99ptotETCCDI", "prcptotETCCDI",
                                    "sdiiETCCDI", "r10mmETCCDI", "r20mmETCCDI", "r1mmETCCDI", "r95ptotETCCDI", "r99ptotETCCDI", "prcptotETCCDI",

                                    "altcddETCCDI", "altcwdETCCDI"),

                         units=c( "days", "days", "days","days", "days",
                                  "days", "days", "days","days",

                                  "degrees_C", "degrees_C", "degrees_C", "degrees_C", "%", "%", "%", "%",
                                  "degrees_C", "degrees_C", "degrees_C", "degrees_C", "%", "%", "%", "%",

                                  "days", "days", "days", "days",

                                  "degrees_C", "mm", "mm",
                                  "degrees_C", "mm", "mm",

                                  "mm d-1", "days", "days", "days", "days", "days", "%", "%", "mm",
                                  "mm d-1", "days", "days", "days", "%", "%", "mm",

                                  "days", "days"),
                         annual=c(T, T, T, T, T,
                                  F, F, F, F,

                                  F, F, F, F, F, F, F, F,
                                  T, T, T, T, T, T, T, T,

                                  T, T, T, T,

                                  F, F, F,
                                  T, T, T,

                                  T, T, T, T, T, T, T, T, T,
                                  F, F, F, F, F, F, F,

                                  T, T),

                         base.period.attr=c(F, F, F, F, F,
                                            F, F, F, F,

                                            F, F, F, F, T, T, T, T,
                                            F, F, F, F, T, T, T, T,

                                            T, T, T, T,

                                            F, F, F,
                                            F, F, F,

                                            F, F, F, F, F, F, T, T, F,
                                            F, F, F, F, T, T, F,

                                            F, F),

                         row.names=c( "fdETCCDI_yr", "suETCCDI_yr", "idETCCDI_yr", "trETCCDI_yr", "gslETCCDI_yr",
                                      "fdETCCDI_mon", "suETCCDI_mon", "idETCCDI_mon", "trETCCDI_mon",

                                      "txxETCCDI_mon", "tnxETCCDI_mon", "txnETCCDI_mon", "tnnETCCDI_mon", "tn10pETCCDI_mon", "tx10pETCCDI_mon", "tn90pETCCDI_mon", "tx90pETCCDI_mon",
                                      "txxETCCDI_yr", "tnxETCCDI_yr", "txnETCCDI_yr", "tnnETCCDI_yr", "tn10pETCCDI_yr", "tx10pETCCDI_yr", "tn90pETCCDI_yr", "tx90pETCCDI_yr",

                                      "wsdiETCCDI_yr", "csdiETCCDI_yr", "altwsdiETCCDI_yr", "altcsdiETCCDI_yr",

                                      "dtrETCCDI_mon", "rx1dayETCCDI_mon", "rx5dayETCCDI_mon",
                                      "dtrETCCDI_yr", "rx1dayETCCDI_yr", "rx5dayETCCDI_yr",

                                      "sdiiETCCDI_yr", "r10mmETCCDI_yr", "r20mmETCCDI_yr", "r1mmETCCDI_yr", "cddETCCDI_yr", "cwdETCCDI_yr", "r95ptotETCCDI_yr", "r99ptotETCCDI_yr", "prcptotETCCDI_yr",
                                      "sdiiETCCDI_mon", "r10mmETCCDI_mon", "r20mmETCCDI_mon", "r1mmETCCDI_mon", "r95ptotETCCDI_mon", "r99ptotETCCDI_mon", "prcptotETCCDI_mon",

                                      "altcddETCCDI_yr", "altcwdETCCDI_yr"),
                         stringsAsFactors=FALSE)

  ## Adding a new variable, explictily adding it as a new row in stead of introducing it in the large data.frame above. This
  ## makes it less likely that we have errors in lining up the variables.
  ## TODO: Refactor the code above to add the rows like below, easier to read.
  all.data = rbind(all.data, r75pETCCDI_yr = data.frame(long.name = 'Annual Total Precipitation when Daily Precipitation Exceeds the 75th Percentile of Wet Day Precipitation', var.name = 'r75pETCCDI', units = 'mm', annual = TRUE, base.period.attr = FALSE),
                   r75pETCCDI_mon = data.frame(long.name = 'Monthly Total Precipitation when Daily Precipitation Exceeds the 75th Percentile of Wet Day Precipitation', var.name = 'r75pETCCDI', units = 'mm', annual = FALSE, base.period.attr = FALSE),
                   r95pETCCDI_yr = data.frame(long.name = 'Annual Total Precipitation when Daily Precipitation Exceeds the 95th Percentile of Wet Day Precipitation', var.name = 'r95pETCCDI', units = 'mm', annual = TRUE, base.period.attr = FALSE),
                   r95pETCCDI_mon = data.frame(long.name = 'Monthly Total Precipitation when Daily Precipitation Exceeds the 95th Percentile of Wet Day Precipitation', var.name = 'r95pETCCDI', units = 'mm', annual = FALSE, base.period.attr = FALSE),
                   r99pETCCDI_yr = data.frame(long.name = 'Annual Total Precipitation when Daily Precipitation Exceeds the 99th Percentile of Wet Day Precipitation', var.name = 'r99pETCCDI', units = 'mm', annual = TRUE, base.period.attr = FALSE),
                   r99pETCCDI_mon = data.frame(long.name = 'Monthly Total Precipitation when Daily Precipitation Exceeds the 99th Percentile of Wet Day Precipitation', var.name = 'r99pETCCDI', units = 'mm', annual = FALSE, base.period.attr = FALSE),
                   r75ptotETCCDI_yr = data.frame(long.name = 'Annual Precipitation fraction due to moderate wet days (daily precipitation exceeds 75th percentile)', var.name = 'r75ptotETCCDI', units = '%', annual = TRUE, base.period.attr = FALSE),
                   r75ptotETCCDI_mon = data.frame(long.name = 'Monthly Precipitation fraction due to moderate wet days (daily precipitation exceeds 75th percentile)', var.name = 'r75ptotETCCDI', units = '%', annual = FALSE, base.period.attr = FALSE),
                   csuETCCDI_yr = data.frame(long.name = 'Annual Number of Consecutive Summer days', var.name = 'csuETCCDI', units = 'days', annual = TRUE, base.period.attr = FALSE),
                   csuETCCDI_mon = data.frame(long.name = 'Monthly Number of Consecutive Summer days', var.name = 'csuETCCDI', units = 'days', annual = FALSE, base.period.attr = FALSE),
                   cfdETCCDI_yr = data.frame(long.name = 'Annual Number of Consecutive Frost days', var.name = 'cfdETCCDI', units = 'days', annual = TRUE, base.period.attr = FALSE),
                   cfdETCCDI_mon = data.frame(long.name = 'Monthly Number of Consecutive Frost days', var.name = 'cfdETCCDI', units = 'days', annual = FALSE, base.period.attr = FALSE),
                   hd17ETCCDI_yr = data.frame(long.name = 'Annual Heating Degree days', var.name = 'hd17ETCCDI', units = 'degrees_C', annual = TRUE, base.period.attr = FALSE),
                   hd17ETCCDI_mon = data.frame(long.name = 'Monthly Heating Degree days', var.name = 'hd17ETCCDI', units = 'degrees_C', annual = FALSE, base.period.attr = FALSE),
                   hiETCCDI_yr = data.frame(long.name = 'Huglin Index', var.name = 'hiETCCDI', units = 'degrees_C', annual = TRUE, base.period.attr = FALSE),
                   spi3ETCCDI_mon = data.frame(long.name = 'Standardized Precipitation Index 3-mon', var.name = 'spi3ETCCDI', units = '', annual = FALSE, base.period.attr = FALSE),
                   spi6ETCCDI_mon = data.frame(long.name = 'Standardized Precipitation Index 6-mon', var.name = 'spi6ETCCDI', units = '', annual = FALSE, base.period.attr = FALSE))


  standard.name.lookup <- c(fdETCCDI="number_frost_days",
                            suETCCDI="number_summer_days",
                            idETCCDI="number_icing_days",
                            trETCCDI="number_tropical_nights",
                            gslETCCDI="growing_season_length",
                            txxETCCDI="maximum_daily_maximum_temperature",
                            tnxETCCDI="maximum_daily_minimum_temperature",
                            txnETCCDI="minimum_daily_maximum_temperature",
                            tnnETCCDI="minimum_daily_minimum_temperature",
                            tn10pETCCDI="percent_days_when_daily_minimum_temperature_below_10p",
                            tx10pETCCDI="percent_days_when_daily_maximum_temperature_below_10p",
                            tn90pETCCDI="percent_days_when_daily_minimum_temperature_above_90p",
                            tx90pETCCDI="percent_days_when_daily_maximum_temperature_above_90p",
                            wsdiETCCDI="warm_spell_duration_index",
                            csdiETCCDI="cold_spell_duration_index",
                            altwsdiETCCDI="warm_spell_duration_index",
                            altcsdiETCCDI="cold_spell_duration_index",
                            dtrETCCDI="diurnal_temperature_range",
                            rx1dayETCCDI="maximum_1day_precipitation",
                            rx5dayETCCDI="maximum_5day_precipitation",
                            sdiiETCCDI="simple_precipitation_intensity_index",
                            r10mmETCCDI="count_days_more_than_10mm_precipitation",
                            r20mmETCCDI="count_days_more_than_20mm_precipitation",
                            r1mmETCCDI="count_days_more_than_1mm_precipitation",
                            cddETCCDI="maximum_number_consecutive_dry_days",
                            cwdETCCDI="maximum_number_consecutive_wet_days",
                            altcddETCCDI="maximum_number_consecutive_dry_days",
                            altcwdETCCDI="maximum_number_consecutive_wet_days",
                            r95ptotETCCDI="precipitation_fraction_exceeding_95th_percentile",
                            r99ptotETCCDI="precipitation_fraction_exceeding_99th_percentile",
                            prcptotETCCDI="total_wet_day_precipitation")

  standard.name.lookup <- c(standard.name.lookup,
                            r75pETCCDI="total_precipitation_exceeding_75th_percentile",
                            r95pETCCDI="total_precipitation_exceeding_95th_percentile",
                            r99pETCCDI="total_precipitation_exceeding_99th_percentile",
                            r75ptotETCCDI="precipitation_fraction_exceeding_75th_percentile",
                            csuETCCDI="consecutive_summer_days",
                            cfdETCCDI="consecutive_frost_days",
                            hd17ETCCDI="heating_degree_days",
                            hiETCCDI="huglin_index",
                            spi3ETCCDI="standardized_precipitation_index 3-mon",
                            spi6ETCCDI="standardized_precipitation_index 6-mon")

  all.data$standard.name <- standard.name.lookup[all.data$var.name]

  all.data$filename <- create.climdex.eobs.filenames(get.split.filename.eobs(template.filename), rownames(all.data))
  return(all.data[vars.list,])
}
