#! /bin/bash 

echo -e '\033[0;31mAdding up tn and tx...\033[0m'
cdo setname,tg -add tn_0.25deg_reg_1950-2016.nc tx_0.25deg_reg_1950-2016.nc /tmp/tmp.nc 
echo -e '\033[0;31mDividing by 2...\033[0m'
cdo divc,2 /tmp/tmp.nc tg_0.25deg_reg_1950-2016.nc
