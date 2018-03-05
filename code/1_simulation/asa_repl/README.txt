The replication archive contains R and Stata scripts as well as datasets to reproduce simulation and applied results in Aronow, Samii, and Assenova, “Cluster Robust Variance Estimation for Dyadic Data.”

R scripts 1-3 reproduce the simulation results.

Stata script 4 reproduces Russett and Oneal’s original estimates and also creates a dataset for our reanalysis.

R script 5 reproduces our reanalysis of Russett and Oneal.

R script 6 reproduces our reanalysis of Fisman et al. 

TRIANGLE.dta are the data for the Russett and Oneal application (obtained from Russett’s homepage: http://pantheon.yale.edu/~brusset/PeaceStata.zip).

Speed Dating Data.csv are the data for the Fisman et al. application (obtained from the website for Gelman and Hill 2007: http://www.stat.columbia.edu/~gelman/arm/).

UPDATE 2016-02-16: 

We have corrected a typo that appeared in the original replication files.  The original files included the line:

Vhat <- dcrUp2 - (length(index)-2)*diag(vcovHC(fit,type="HC0"))

This has been corrected as:

Vhat <- dcrUp2 - (length(index)-2)*vcovHC(fit,type="HC0")

The typo did not affect the results reported in the paper, however it does affect the results for the off diagonals in the Vhat matrix, and therefore may be important for those wishing to use Vhat to test restrictions on sets of coefficients.

We than Max Goplerud for bringing this to our attention.