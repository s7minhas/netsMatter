Instructions for WeeksAMEN_general.R

the AMENConfig.sh script takes three input arguments (integers): latent dimensions, imps, and burn.


AMENConfig.sh first adds the time and configuration parameters to the logfile.txt, then runs the AMEN package for the input parameters, finally  prints out diagnostic plots for beta, GOF, and VC.

Useage example
./AMENConfig.sh 1 100 200


