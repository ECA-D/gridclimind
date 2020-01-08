#! /bin/bash 

function subset {
    cdo sellonlatbox,${1} ../E-OBS_grids/${2} ${2}
}

function subset_threshold {
    cdo -selname,${2} ../E-OBS_grids/${3} /tmp/tmp1.nc
    cdo -sellonlatbox,${1} /tmp/tmp1.nc ${3}
}


bbox=6,7,49,50

echo Removing nc files
rm *nc

echo Subsetting rainfall
subset $bbox rr_0.25deg_reg_1950-2016.nc

echo Subsetting minimum temperature
subset $bbox tn_0.25deg_reg_1950-2016.nc

echo Subsetting maximum temperature
subset $bbox tx_0.25deg_reg_1950-2016.nc

echo Subsetting temperature threshold EOBS_TH_TX_TN_v14
subset_threshold $bbox tx10thresh,tx90thresh,tn10thresh,tn90thresh EOBS_TH_TX_TN_v14.nc

echo Subsetting rainfall threshold
subset_threshold $bbox r75thresh,r95thresh,r99thresh EOBS_TH_rr_v14.nc

echo Subsetting temperature threshold EOBSv14_TH_tx_tn_10_90
subset_threshold $bbox tx10thresh,tx90thresh,tn10thresh,tn90thresh EOBSv14_TH_tx_tn_10_90.nc
