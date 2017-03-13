#!/bin/bash

## Script to create AMEN config files
## will echo config settings into
## logfile. 

## this is competely duplicated from the Weeks models

## takes command line arguments
## in sequence: num lat dims, imps, burn
## config is an .R file, so objects declared

echo "latDims= $1" > config.R
echo "imps= $2" >> config.R
echo "brn= $3" >> config.R

## now keep track of settings into the logfile:
echo "$(date '+%D-%H:%M:%S')" >> logfile.txt
cat config.R >> logfile.txt
echo "--" >>logfile.txt
