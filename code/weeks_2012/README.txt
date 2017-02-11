Instructions for WeeksAMEN_general.R

the AMENConfig.sh script takes three input arguments (integers): latent dimensions, imps, and burn.


AMENConfig.sh first adds the time and configuration parameters to the logfile.txt, then runs the AMEN package for the input parameters, finally  prints out diagnostic plots for beta, GOF, and VC.

Template:

AMENConfig.sh latDems nscan burnin 
Useage example

./AMENConfig.sh 1 100 200 

## First pass:

./AMENConfig.sh 1 5000 10000 


### Notes on running long chains:

1) When need to install packages, run those in a separate R script. Such as:

[terminal->]

sudo R --vanilla
[when buffer starts]
install.packages(c("..."))
quit()

Otherwise, if you run the script as sudo, it will make the Sys.info user 'root'. Observe that this is a non-unique username, so don't want to
hardcode too many things if on root.

## what I did on 2/10:

Starting parallel screen process to run based on # of latent dimensions from 0:3.

#generate config:

- ./AMENConfig.sh LD  100000  500000
-  run AMEN via screen
- wait until burn-in period starts, then start the process for the next latent dimension

## As of 12:45 on Feb 10, had latent dims running for 0:2 latent dimensions
## Check up on the system usage in 10 hours or so, when it is moving out of burn-in
