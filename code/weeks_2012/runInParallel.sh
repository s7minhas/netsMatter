#!/bin/bash

## call the four AMEN processe
## ...hopefully in parallel


parallel --j 4 ./AMENConfig.sh  ::: '0 5000 10000 25' '1 5000 10000 25' '2 5000 10000 25' '3 5000 10000 25'
