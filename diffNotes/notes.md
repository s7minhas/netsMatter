I have submitted the replication archive for "Taking Dyads Seriously" (PSRM-OA-2019-0126) to the PSRM dataverse. Sincerest apologies for all the delays. Building the replication archive took an embarrassingly long time because of just work/life these past few months and problems that I found in the code for the application portion of this project (all the simulation results remain exactly the same). I wanted to make sure that I took the time to understand how these problems manifested. The changes lead to only minor revisions in the text, each of which are highlighted in the diff.pdf that I've attached, see pages: 19, 20, 22, and 24. 

The most important mistake is with the out-of-sample GLM cross-validation analysis. For Weeks and Gibler, there were errors in the code, and when fixing them, we see an increase the AUC-ROC/PR statistics for the models the authors originally used. The AME still markedly outperforms, but the margin is narrower. The updated figure is included in the diffs.pdf, and I have attached a screenshot of the older version (old_figure6.PNG).

There were a few other minor changes as well: 

- Figure 7 results change slightly because there were some errors in country matching.
- Figure 8 may look a bit different but results are actually exactly the same just reoriented. This occurs because when creating the visualization of the multiplicative effects, we use a Procrustes transformation across the posterior samples using the first sample as a reference point. The relative comparisons that one would make from the figure are the same as what was originally submitted.
- Figure 9 results change in that the predictions of a one-unit change in the rivalry variable are much smaller now than the predictions from the GLM model. This is because the Gibler model results reported in the submitted version hadn't fully converged, in particular, the intercept parameter. This has been fixed in the replication archive.

I completely accept responsibility for each of these mistakes and the extreme tardiness in submitting the replication archive for review. If you decide that you can no longer accept the manuscript given any of the changes I referenced above, I will completely understand.

Best, 

Shahryar