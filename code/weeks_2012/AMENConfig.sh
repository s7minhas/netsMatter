#!/bin/bash

## Script to start AMEN runs
## will echo config settings into
## config.txt

## takes command line arguments
## in sequence: num lat dims, imps, burn
## config is an .R file, so objects declared

echo "latDims=$1" > config.R
echo "imps=$2" >> config.R
echo "brn=$3" >> config.R

## now keep track of settings into the logfile:
echo "$(date '+%Y%M%d-%H:%M:%S')" >> logfile.txt
cat config.R >> logfile.txt
echo "--" >>logfile.txt

## run AMEN

Rscript WeeksAMEN_general.R

