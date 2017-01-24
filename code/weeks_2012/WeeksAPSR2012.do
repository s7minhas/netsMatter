*Replication of Weeks 2012 APSR

clear
set mem 4000m
use WeeksAPSR2012.dta

sum mzinit machinejlw_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin contigdum logdist s_wt_glo s_lead_1 s_lead_2 pcyrsmzinit pcyrsmzinits* democracy_1 dirdyadid, d

* TABLE 1
	* 1.1
logit mzinit machinejlw_1  juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 cap_1 cap_2 majmaj minmaj majmin pcyrsmzinit pcyrsmzinits* if democracy_1!=., robust cluster(dirdyadid)
testparm machinejlw_1 juntajlw_1, equal
testparm machinejlw_1 bossjlw_1, equal
testparm machinejlw_1 strongmanjlw_1, equal
testparm juntajlw_1 strongmanjlw_1, equal
testparm bossjlw_1 strongmanjlw_1, equal

	* 1.2
logit mzinit machinejlw_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin contigdum logdist s_wt_glo s_lead_1 s_lead_2 pcyrsmzinit pcyrsmzinits* if democracy_1!=., robust cluster(dirdyadid)
testparm machinejlw_1 juntajlw_1, equal
testparm machinejlw_1 bossjlw_1, equal
testparm machinejlw_1 strongmanjlw_1, equal
testparm juntajlw_1 strongmanjlw_1, equal
testparm bossjlw_1 strongmanjlw_1, equal

	* also did with time, time2, time 3 (Carter and Signorino 2010) - no difference 
logit mzinit machinejlw_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin contigdum logdist s_wt_glo s_lead_1 s_lead_2 time time2 time3 if democracy_1!=., robust cluster(dirdyadid)
	
	* FE models (directed-dyad )
xtset dirdyadid year

	* 1.3 parsimonious specification, and drop time-invariant variables such as distance, contiguity, and alliance portfolio
xtlogit mzinit      machinejlw_1 juntajlw_1 bossjlw_1  strongmanjlw_1 allotherauts_1  cap_1 cap_2 majmaj minmaj majmin pcyrsmzinit pcyrsmzinits* if democracy_1!=., fe
testparm machinejlw_1 juntajlw_1, equal
testparm machinejlw_1 bossjlw_1, equal
testparm machinejlw_1 strongmanjlw_1, equal
testparm juntajlw_1 strongmanjlw_1, equal
testparm bossjlw_1 strongmanjlw_1, equal

	* 1.4 drop time-invariant variables such as distance, contiguity, and alliance portfolio
xtlogit mzinit      machinejlw_1 juntajlw_1 bossjlw_1  strongmanjlw_1 allotherauts_1  newregime_1 cap_1 cap_2 initshare dependlow majmaj minmaj majmin pcyrsmzinit pcyrsmzinits* if democracy_1!=., fe
testparm machinejlw_1 juntajlw_1, equal
testparm machinejlw_1 bossjlw_1, equal
testparm machinejlw_1 strongmanjlw_1, equal
testparm juntajlw_1 strongmanjlw_1, equal
testparm bossjlw_1 strongmanjlw_1, equal


* TABLE 2 *
	* Table 2.1 - cross-sectional, among autocratic regimes [drop democracies and autocracies without raw data]
	* cross-sectional model
logit mzinit persrat_1 milrat_1 persXmil newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin contigdum logdist s_wt_glo s_lead_1 s_lead_2 pcyrsmzinit pcyrsmzinits* if democracy_1==0, robust cluster(dirdyadid)

	* Table 2.2 - FE with raw scores, among autocratic regimes
	* had to drop dependlow bc was not concave
xtlogit mzinit persrat_1 milrat_1 persXmil 										      newregime_1 cap_1 cap_2 initshare majmaj minmaj majmin pcyrsmzinit pcyrsmzinits* if democracy_1==0 & party_1!=., fe

	* Table 2.3
	* cross-sectional model, assigning a "0" for democracies so that they are not dropped
logit mzinit persrat_1a milrat_1 persXmila newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin contigdum logdist s_wt_glo s_lead_1 s_lead_2 pcyrsmzinit pcyrsmzinits* if democracy_1!=. & regimejlw_1<6, robust cluster(dirdyadid)

	* Table 2.4
	* FE with raw scores, , assigning a "0" for democracies so that they are not dropped
	* had to drop dependlow bc was not concave
xtlogit mzinit persrat_1a milrat_1 persXmila 										      newregime_1 cap_1 cap_2 initshare majmaj minmaj majmin pcyrsmzinit pcyrsmzinits*, fe


* Figure 4 - Substantive Effects
set more off

* Use Model 1.2
* Set a seed for the simulations so that the results are identical for replication purposes
set seed 20110711
estsimp logit mzinit machinejlw_1  juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin contigdum logdist s_wt_glo s_lead_1 s_lead_2 pcyrsmzinit pcyrsmzinits* if democracy_1!=., robust cluster(dirdyadid)

* Counterfactual scenario: model a dyad that looks like Iraq vs. Kuwait in 1990 - stronger country facing weak neighbor, etc - and then vary regime type of Side A
list mzinit machinejlw_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin contigdum logdist s_wt_glo s_lead_1 s_lead_2 pcyrsmzinit pcyrsmzinits* if abbrev1=="IRQ" & abbrev2=="KUW" & year==1990
setx median
setx machinejlw_1 0 juntajlw_1 0 bossjlw_1 0 strongmanjlw_1 0 allotherauts_1 0 democracy_2 0 newregime_1 0 democracy_2 0 cap_1 .012707 cap_2 .003571 initshare .7806242 dependlow  .0069602 majmaj 0 minmaj 0 majmin 0 contigdum 1 logdist 0 s_wt_glo .914585 s_lead_1 .10303 s_lead_2 .09899 pcyrsmzinit 5 pcyrsmzinits1  -120.2186  pcyrsmzinits2 -112.7049 pcyrsmzinits3 -103.1421 

* Figure 4 simply graphs the output below - the predicted values and simulated c.i's for each regime type
	* Democracy
simqi
	* Machine
setx machinejlw_1 1 
simqi
	* Junta
setx machinejlw_1 0 juntajlw_1 1
simqi
	* Boss
setx juntajlw_1 0 bossjlw_1 1
simqi
	* Strongman
setx bossjlw_1 0 strongmanjlw_1 1
simqi

* Figure 3 - The Interaction Between Personalism and Militarism
* what effect does a unit change in personalism have on the probability of initiation when militarism is held constant at different values
set more off
gen persrat=persrat_1a
gen milrat=milrat_1
	* Marginal effect of PERSONALISM using 2.1
logit mzinit c.persrat_1##c.milrat newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin contigdum logdist s_wt_glo s_lead_1 s_lead_2 pcyrsmzinit pcyrsmzinits* if democracy_1!=. & regimejlw_1<6, robust cluster(dirdyadid)
tab milrat if e(sample)
margins, dydx(persrat_1) at(milrat=(0(.05)1)) vsquish post
matrix at=e(at)
matrix at=at[1...,"milrat"]
matrix list at
parmest, norestore
svmat at
twoway (line min95 at1, lpattern(dash)) (line estimate at1) (line max95 at1, lpattern(dash)), legend(order (1 "Upper 95% c.i." 3 "Lower 95% c.i.")) yline(0) ///
       xtitle(Score on militarism index) ytitle(Marginal effect of personalism) scheme(s1mono) yscale(range(-.001 0.004)) ylabel(#5)
save margpersat_1.dta, replace

clear
use WeeksAPSR2012.dta
set more off
gen persrat=persrat_1a
gen milrat=milrat_1
	* Marginal effect of MILITARISM using 2.1
logit mzinit c.persrat_1##c.milrat newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin contigdum logdist s_wt_glo s_lead_1 s_lead_2 pcyrsmzinit pcyrsmzinits* if democracy_1!=. & regimejlw_1<6, robust cluster(dirdyadid)
tab persrat if e(sample)
margins, dydx(milrat) at(persrat_1=(0(.05)1)) vsquish post
matrix at=e(at)
matrix at=at[1...,"persrat_1"]
matrix list at
parmest, norestore
svmat at
twoway (line min95 at1, lpattern(dash)) (line estimate at1) (line max95 at1, lpattern(dash)), legend(order (1 "Upper 95% c.i." 3 "Lower 95% c.i.")) yline(0) ///
       xtitle(Score on personalism index) ytitle(Marginal effect of militarism)	scheme(s1mono) yscale(range(-.001 0.004)) ylabel(#5)
save margmilrat_1.dta, replace
exit


* Robustness checks
clear
use WeeksAPSR2012.dta
xtset dirdyadid year

* basic specifications as a baseline
* Model 1.2
logit mzinit machinejlw_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 cap_1 cap_2 initshare dependlow       majmaj minmaj majmin contigdum logdist s_wt_glo s_lead_1 s_lead_2 pcyrsmzinit pcyrsmzinits* if democracy_1!=., robust cluster(dirdyadid)
	outreg using robust2, replace

* Model 1.4
xtlogit mzinit machinejlw_1 juntajlw_1 bossjlw_1  strongmanjlw_1 allotherauts_1  newregime_1 cap_1 cap_2 initshare dependlow majmaj minmaj majmin pcyrsmzinit pcyrsmzinits* if democracy_1!=., fe
	outreg using robust2, append
	
	* TABLE A-3-1
	* 0 "The results do not change if each country's trade depdence is entered separately" 
logit mzinit machinejlw_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 cap_1 cap_2 initshare depend1 depend2 majmaj minmaj majmin contigdum logdist s_wt_glo s_lead_1 s_lead_2 pcyrsmzinit pcyrsmzinits* if democracy_1!=., robust cluster(dirdyadid)
	outreg using robust2, append

xtlogit mzinit  machinejlw_1 juntajlw_1 bossjlw_1  strongmanjlw_1 allotherauts_1  newregime_1 cap_1 cap_2 initshare depend1 depend2        majmaj minmaj majmin pcyrsmzinit pcyrsmzinits* if democracy_1!=., fe
	outreg using robust2, append

	* 1 - "Including Side B regime type in the model to ensure that certain regime types were not disproportionately likely to have neighbors that incited more MIDs."
logit mzinit machinejlw_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 juntajlw_2 bossjlw_2 strongmanjlw_2 allotherauts_2 newregime_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin contigdum logdist s_wt_glo s_lead_1 s_lead_2 pcyrsmzinit pcyrsmzinits* if democracy_1!=., robust cluster(dirdyadid)
	outreg using robust2, append
	
xtlogit mzinit machinejlw_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 juntajlw_2 bossjlw_2 strongmanjlw_2 allotherauts_2 newregime_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin pcyrsmzinit pcyrsmzinits* if democracy_1!=., fe
	outreg using robust2, append
	
	
	* TABLE A-3-2
	* 2 - "Dropping the Warsaw Pact countries (other than the USSR) from the sample, since their foreign policies might not have been truly independent."
logit mzinit machinejlw_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin contigdum logdist s_wt_glo s_lead_1 s_lead_2 pcyrsmzinit pcyrsmzinits* if democracy_1!=. & warsaw_1!=1, robust cluster(dirdyadid)
	outreg using robust3, replace

xtlogit mzinit      machinejlw_1 juntajlw_1 bossjlw_1  strongmanjlw_1 allotherauts_1  newregime_1 cap_1 cap_2 initshare dependlow majmaj minmaj majmin pcyrsmzinit pcyrsmzinits* if democracy_1!=. & warsaw_1!=1, fe
	outreg using robust3, append
	
	* 3 - restrict the sample only to dyads that are not allied 
logit mzinit machinejlw_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin contigdum logdist s_wt_glo s_lead_1 s_lead_2 pcyrsmzinit pcyrsmzinits* if democracy_1!=. & allied==0, robust cluster(dirdyadid)
	outreg using robust3, append

xtlogit mzinit      machinejlw_1 juntajlw_1 bossjlw_1  strongmanjlw_1 allotherauts_1  newregime_1 cap_1 cap_2 initshare dependlow majmaj minmaj majmin pcyrsmzinit pcyrsmzinits* if democracy_1!=. & allied==0, fe
	outreg using robust3, append


	* Table A-3-3
	* 4 - "Dropping individual countries, such as the USSR, China, Iraq, and the U.S. from the sample, both individually and in various combinations."
	* don't forget to testparm among each of the authoritarian regime types
	* Drop China
logit mzinit machinejlw_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin contigdum logdist s_wt_glo s_lead_1 s_lead_2 pcyrsmzinit pcyrsmzinits* if democracy_1!=. & ccode1!=710, robust cluster(dirdyadid)
outreg using robust4, replace
	* Drop Iraq
logit mzinit machinejlw_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin contigdum logdist s_wt_glo s_lead_1 s_lead_2 pcyrsmzinit pcyrsmzinits* if democracy_1!=. & ccode1!=645, robust cluster(dirdyadid)
outreg using robust4, append
	* Drop USSR
logit mzinit machinejlw_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin contigdum logdist s_wt_glo s_lead_1 s_lead_2 pcyrsmzinit pcyrsmzinits* if democracy_1!=. & ccode1!=365, robust cluster(dirdyadid)
outreg using robust4, append
	* Drop all three
logit mzinit machinejlw_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin contigdum logdist s_wt_glo s_lead_1 s_lead_2 pcyrsmzinit pcyrsmzinits* if democracy_1!=. & ccode1!=710 & ccode1!=645 & ccode1!=365, robust cluster(dirdyadid)
	outreg using robust4, append
	
	* With FE: "Dropping individual countries, such as the USSR, China, Iraq, and the U.S. from the sample, both individually and in various combinations."
	* Drop China
xtlogit mzinit machinejlw_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin pcyrsmzinit pcyrsmzinits* if democracy_1!=. & ccode1!=710, fe
outreg using robust4, append
	* Drop Iraq
xtlogit mzinit machinejlw_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin pcyrsmzinit pcyrsmzinits* if democracy_1!=. & ccode1!=645, fe
outreg using robust4, append
	* Drop USSR
xtlogit mzinit machinejlw_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin pcyrsmzinit pcyrsmzinits* if democracy_1!=. & ccode1!=365, fe
	* Drop all three
xtlogit mzinit machinejlw_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin pcyrsmzinit pcyrsmzinits* if democracy_1!=. & ccode1!=710 & ccode1!=645 & ccode1!=365, fe
	outreg using robust4, append

	* Table A-3-4
	* 5 - Estimating models that control for Polity scores and/or dropping anocracies
			*( regimes with Polity scores between -5 and +5) from the sample to ensure that 
			* machines are not simply the "most democratic" of the authoritarians.
	* machine is base category
	* cross-sectional
logit mzinit polity2_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin contigdum logdist s_wt_glo s_lead_1 s_lead_2 pcyrsmzinit pcyrsmzinits* if democracy_1==0, robust cluster(dirdyadid)
outreg using robust5, replace
	
	* with FE
	* have to drop other vars (dependlow)- otherwise won't converge 
xtlogit mzinit polity2_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 cap_1 cap_2 initshare majmaj minmaj majmin pcyrsmzinit pcyrsmzinits* if democracy_1==0, fe
outreg using robust5, append

	* drop anocracies from the sample to make sure they are not driving the results
	* cross-sectional
logit mzinit machinejlw_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin contigdum logdist s_wt_glo s_lead_1 s_lead_2 pcyrsmzinit pcyrsmzinits* if (polity2_1<-5 | polity2_1>5) & democracy_1!=., robust cluster(dirdyadid)
outreg using robust5, append

	* with FE
xtlogit mzinit      machinejlw_1 juntajlw_1 bossjlw_1  strongmanjlw_1 allotherauts_1  newregime_1 cap_1 cap_2 initshare majmaj minmaj majmin pcyrsmzinit pcyrsmzinits* if (polity2_1<-5 | polity2_1>5) & democracy_1!=., fe
outreg using robust5, append
		
		
	* Table A-3-5
	* 6 - restrict the sample to only minor powers on Side 1
	* cross-sectional
logit mzinit machinejlw_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin contigdum logdist s_wt_glo s_lead_1 s_lead_2 pcyrsmzinit pcyrsmzinits* if democracy_1!=. & majmin!=1 & majmaj!=1, robust cluster(dirdyadid)
outreg using robust6, replace

	* with FE
xtlogit mzinit      machinejlw_1 juntajlw_1 bossjlw_1  strongmanjlw_1 allotherauts_1  newregime_1 cap_1 cap_2 initshare dependlow majmaj minmaj majmin pcyrsmzinit pcyrsmzinits* if majmin!=1 & majmaj!=1 & democracy_1!=., fe
outreg using robust6, append

	* 7 - control for regions (cross-sectional only)
logit mzinit machinejlw_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin contigdum logdist s_wt_glo s_lead_1 s_lead_2 region1d* pcyrsmzinit pcyrsmzinits* if democracy_1!=., robust cluster(dirdyadid)
outreg using robust6, append

	* 8 - control for civil war
logit mzinit machinejlw_1 juntajlw_1 bossjlw_1 strongmanjlw_1 allotherauts_1 newregime_1 democracy_2 cap_1 cap_2 initshare dependlow majmaj minmaj majmin civilwar_1 civilwar_2 contigdum logdist s_wt_glo s_lead_1 s_lead_2 pcyrsmzinit pcyrsmzinits* if democracy_1!=., robust cluster(dirdyadid)
	outreg using robust6, append

xtlogit mzinit      machinejlw_1 juntajlw_1 bossjlw_1  strongmanjlw_1 allotherauts_1  newregime_1 cap_1 cap_2 initshare dependlow majmaj minmaj majmin civilwar_1 civilwar_2 pcyrsmzinit pcyrsmzinits* if majmin!=1 & majmaj!=1 & democracy_1!=., fe
	outreg using robust6, append


