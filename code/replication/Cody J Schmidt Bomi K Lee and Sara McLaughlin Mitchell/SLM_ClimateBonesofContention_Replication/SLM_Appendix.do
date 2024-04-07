*** SLM, "Climate Bones of Contention" Appendix

** Table A1.  Summary Statistics 
use "SLM_Onset_19012001.dta", clear 

sum terriss riveriss mariss onset s_pre_chal s_tmp_chal vol_pre_chal vol_tmp_chal s_pre_tgt  ///
s_tmp_tgt vol_pre_tgt vol_tmp_tgt cap_relative joint_dem lgdist iss_peaceyr

** Table A2.  Correlations.
corr onset s_pre_chal s_tmp_chal vol_pre_chal vol_tmp_chal s_pre_tgt s_tmp_tgt vol_pre_tgt vol_tmp_tgt 


use "SLM_Militarization_19012001.dta", clear

** Table A1.  Summary Statistics 
sum midissyr s_pre_chal s_tmp_chal  vol_pre_chal vol_tmp_chal  s_pre_tgt s_tmp_tgt vol_pre_tgt vol_tmp_tgt ///
cap_relative joint_dem lgdist mid_peaceyr

** Table A2.  Correlations.
corr midissyr s_pre_chal s_tmp_chal vol_pre_chal vol_tmp_chal s_pre_tgt s_tmp_tgt vol_pre_tgt vol_tmp_tgt 


** Margins for Issue Specific Models

*Figure A1
use "SLM_Onset_19012001.dta", clear 

qui logit terriss c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal  ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt   ///
	cap_relative joint_dem lgdist iss_peaceyr, robust

margins, at(s_pre_chal=(-2.5(0.5)2.5)) predict(xb) saving(onset_terr)

use onset_terr, clear
foreach v of varlist _margin _ci_lb _ci_ub {
    replace `v' = invlogit(`v')
}

twoway  ///
    (rcap _ci_lb _ci_ub _at2, sort pstyle(ci) color(gs7) lw(median)) ///
	(connected _margin _at2, sort color(black)), ///
    xlabel(-2.5(.5)2.5, labsize(small)) ///
	ylabel(0(.001)0.003, labsize(small)) ///
	xtitle("Challenger's Precipitation") ///
	ytitle("Pr(Territory Issue Onset)" "") ///
	graphregion(color(white)) ///
	title(" ") ///
	legend(off) ///
	note("Note: 95% Confidence Intervals reported.", size(small)) ///
	saving(onset_ter_pre.gph, replace)
	

* Figure A2
use "SLM_Onset_19012001.dta", clear 

qui logit riveriss c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt  ///
    cap_relative joint_dem lgdist iss_peaceyr, robust

margins, at(s_pre_tgt=(-2.5(0.5)2.5)) predict(xb) saving(onset_river)

use onset_river, clear
foreach v of varlist _margin _ci_lb _ci_ub {
    replace `v' = invlogit(`v')
}
twoway  ///
    (rcap _ci_lb _ci_ub _at, sort pstyle(ci) color(gs7) lw(median)) ///
	(connected _margin _at, sort color(black)), ///
    xlabel(1 "-2.5" 2 "-2" 3 "-1.5" 4 "-1" 5 "-0.5" 6 "0" 7 "0.5" 8 "1" 9 "1.5" ///
	10 "2" 11 "2.5", labsize(small)) ///
	ylabel(0(.0001)0.0003, labsize(small)) ///
	xtitle("Target's Precipitation") ///
	ytitle("Pr(River Issue Onset)" "") ///
	graphregion(color(white)) ///
	title(" ") ///
	legend(off) ///
	note("Note: 95% Confidence Intervals reported.", size(small)) ///
	saving(onset_riv_pre.gph, replace)

* Figure A3  
use "SLM_Onset_19012001.dta", clear 

qui logit mariss c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt   ///
    cap_relative joint_dem lgdist iss_peaceyr, robust

margins, at(s_pre_chal=(-2.5(0.5)2.5)) predict(xb) saving(onset_maritime_pre)
margins, at(s_tmp_chal=(-2.5(0.5)2.5)) predict(xb) saving(onset_maritime_tmp)
margins, at(s_pre_tgt=(-2.5(0.5)2.5)) predict(xb) saving(onset_maritime_tgt)

* Figure A3a
use onset_maritime_pre, clear
foreach v of varlist _margin _ci_lb _ci_ub {
    replace `v' = invlogit(`v')
}
twoway  ///
    (rcap _ci_lb _ci_ub _at2, sort pstyle(ci) color(gs7) lw(median)) ///
	(connected _margin _at2, sort color(black)), ///
    xlabel(-2.5(.5)2.5, labsize(small)) ///
	ylabel(0(.001)0.005, labsize(small)) ///
	xtitle("Challenger's Precipitation") ///
	ytitle("Pr(Maritime Issue Onset)" "") ///
	graphregion(color(white)) ///
	title(" ") ///
	legend(off) ///
	note("Note: 95% Confidence Intervals reported.", size(small)) ///
	saving(onset_mar_pre.gph, replace)

* Figure A3b
use onset_maritime_tmp, clear
foreach v of varlist _margin _ci_lb _ci_ub {
    replace `v' = invlogit(`v')
}
twoway  ///
    (rcap _ci_lb _ci_ub _at1, sort pstyle(ci) color(gs7) lw(median)) ///
	(connected _margin _at1, sort color(black)), ///
    xlabel(-2.5(.5)2.5, labsize(small)) ///
	ylabel(0(.001)0.0025, labsize(small)) ///
	xtitle("Challenger's Temperature") ///
	ytitle("Pr(Maritime Issue Onset)" "") ///
	graphregion(color(white)) ///
	title(" ") ///
	legend(off) ///
	note("Note: 95% Confidence Intervals reported.", size(small)) ///
	saving(onset_mar_tmp.gph, replace)

* Figure A3c
use onset_maritime_tgt, clear
foreach v of varlist _margin _ci_lb _ci_ub {
    replace `v' = invlogit(`v')
}
twoway  ///
    (rcap _ci_lb _ci_ub _at4, sort pstyle(ci) color(gs7) lw(median)) ///
	(connected _margin _at4, sort color(black)), ///
    xlabel(-2.5(.5)2.5, labsize(small)) ///
	ylabel(0(.001)0.004, labsize(small)) ///
	xtitle("Target's Precipitation") ///
	ytitle("Pr(Maritime Issue Onset)" "") ///
	graphregion(color(white)) ///
	title(" ") ///
	legend(off) ///
	note("Note: 95% Confidence Intervals reported.", size(small)) ///
	saving(onset_mar_tgt.gph, replace)

* Figure A4
use "SLM_Onset_19012001.dta", clear 

qui logit mariss vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt  ///
    cap_relative joint_dem lgdist iss_peaceyr, robust
	
margins, at(vol_tmp_chal=(-2.5(0.5)2.5)) predict(xb) saving(onset_mar_vol)

use onset_mar_vol, clear
foreach v of varlist _margin _ci_lb _ci_ub {
    replace `v' = invlogit(`v')
}

twoway  ///
    (rcap _ci_lb _ci_ub _at2, sort pstyle(ci) color(gs7) lw(median)) ///
	(connected _margin _at2, sort color(black)), ///
    xlabel(-2.5(.5)2.5, labsize(small)) ///
	ylabel(0(.001)0.005, labsize(small)) ///
	xtitle("Challenger's Temperature Volatility") ///
	ytitle("Pr(Maritme Issue Onset)" "") ///
	graphregion(color(white)) ///
	title(" ") ///
	legend(off) ///
	note("Note: 95% Confidence Intervals reported.", size(small)) ///
	saving(onset_mar_vol.gph, replace)
	
* Figure A5
use "SLM_Militarization_19012001.dta", clear

qui logit midissyr c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal   ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt  ///
	cap_relative joint_dem lgdist mid_peaceyr if terriss==1, robust

margins, at(s_pre_chal=(-2.5(0.5)2.5)) predict(xb) saving(mil_ter_chal)
margins, at(s_pre_tgt=(-2.5(0.5)2.5)) predict(xb) saving(mil_ter_tgt)

* Figure A5a
use mil_ter_chal, clear
foreach v of varlist _margin _ci_lb _ci_ub {
    replace `v' = invlogit(`v')
}

twoway  ///
    (rcap _ci_lb _ci_ub _at2, sort pstyle(ci) color(gs7) lw(median)) ///
	(connected _margin _at2, sort color(black)), ///
    xlabel(-2.5(.5)2.5, labsize(small)) ///
	ylabel(0(.05)0.2, labsize(small)) ///
	xtitle("Challenger's Precipitation") ///
	ytitle("Pr(Territory Issue Militarization)" "") ///
	graphregion(color(white)) ///
	title(" ") ///
	legend(off) ///
	note("Note: 95% Confidence Intervals reported.", size(small)) ///
	saving(mil_ter_chal.gph, replace)

* Figure A5b
	
use mil_ter_tgt, clear
foreach v of varlist _margin _ci_lb _ci_ub {
    replace `v' = invlogit(`v')
}

twoway  ///
    (rcap _ci_lb _ci_ub _at4, sort pstyle(ci) color(gs7) lw(median)) ///
	(connected _margin _at4, sort color(black)), ///
    xlabel(-2.5(.5)2.5, labsize(small)) ///
	ylabel(0(.03)0.1, labsize(small)) ///
	xtitle("Target's Precipitation") ///
	ytitle("Pr(Territory Issue Militarization)" "") ///
	graphregion(color(white)) ///
	title(" ") ///
	legend(off) ///
	note("Note: 95% Confidence Intervals reported.", size(small)) ///
	saving(mil_ter_tgt.gph, replace)	

* Figure A6
use "SLM_Militarization_19012001.dta", clear

qui logit midissyr c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt ///
    cap_relative joint_dem lgdist mid_peaceyr if riveriss==1, robust

margins, at(s_tmp_chal=(-2.5(0.5)2.5)) predict(xb) saving(mil_riv_tmp)

use mil_riv_tmp, clear
foreach v of varlist _margin _ci_lb _ci_ub {
    replace `v' = invlogit(`v')
}

twoway  ///
    (rcap _ci_lb _ci_ub _at1, sort pstyle(ci) color(gs7) lw(median)) ///
	(connected _margin _at1, sort color(black)), ///
    xlabel(-2.5(.5)2.5, labsize(small)) ///
	ylabel(0(.2)1, labsize(small)) ///
	xtitle("Challenger's Temperature") ///
	ytitle("Pr(River Issue Militarization)" "") ///
	graphregion(color(white)) ///
	title(" ") ///
	legend(off) ///
	note("Note: 95% Confidence Intervals reported.", size(small)) ///
	saving(mil_riv_tmp.gph, replace)
	
* Figure A7
use "SLM_Militarization_19012001.dta", clear

qui logit midissyr vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt ///
    cap_relative joint_dem lgdist mid_peaceyr if riveriss==1, robust

margins, at(vol_tmp_chal=(-2.5(0.5)2.5)) predict(xb) saving(mil_riv_vol)

use mil_riv_vol, clear
foreach v of varlist _margin _ci_lb _ci_ub {
    replace `v' = invlogit(`v')
}
twoway  ///
    (rcap _ci_lb _ci_ub _at2, sort pstyle(ci) color(gs7) lw(median)) ///
	(connected _margin _at2, sort color(black)), ///
    xlabel(-2.5(.5)2.5, labsize(small)) ///
	ylabel(0(.1)0.5, labsize(small)) ///
	xtitle("Challenger's Temperature Volatility") ///
	ytitle("Pr(River Issue Militarization)" "") ///
	graphregion(color(white)) ///
	title(" ") ///
	legend(off) ///
	note("Note: 95% Confidence Intervals reported.", size(small)) ///
	saving(mil_riv_vol.gph, replace)
	
* Table A3a: Western Hemisphere
use "SLM_Onset_19012001.dta" 

logit terriss c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal  ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt   ///
	cap_relative joint_dem lgdist iss_peaceyr if chal<200& tgt<200, robust

logit riveriss c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt  ///
    cap_relative joint_dem lgdist iss_peaceyr if chal<200& tgt<200, robust

logit mariss c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt   ///
    cap_relative joint_dem lgdist iss_peaceyr if chal<200& tgt<200, robust

logit onset c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt  ///
	cap_relative joint_dem lgdist iss_peaceyr if chal<200& tgt<200, robust

logit terriss vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt   ///
	cap_relative joint_dem lgdist iss_peaceyr if chal<200& tgt<200, robust

logit riveriss vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt  ///
    cap_relative joint_dem lgdist iss_peaceyr if chal<200& tgt<200, robust

logit mariss vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt   ///
    cap_relative joint_dem lgdist iss_peaceyr if chal<200& tgt<200, robust

logit onset vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt  ///
	cap_relative joint_dem lgdist iss_peaceyr if chal<200& tgt<200, robust


* Table A3b: Europe
logit terriss c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal  ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt   ///
	cap_relative joint_dem lgdist iss_peaceyr if chal>=200& tgt>=200, robust

logit riveriss c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt  ///
    cap_relative joint_dem lgdist iss_peaceyr if chal>=200& tgt>=200, robust

logit mariss c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt   ///
    cap_relative joint_dem lgdist iss_peaceyr if chal>=200& tgt>=200, robust

logit onset c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt  ///
	cap_relative joint_dem lgdist iss_peaceyr if chal>=200& tgt>=200, robust

logit terriss vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt   ///
	cap_relative joint_dem lgdist iss_peaceyr if chal>=200& tgt>=200, robust

logit riveriss vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt  ///
    cap_relative joint_dem lgdist iss_peaceyr if chal>=200& tgt>=200, robust

logit mariss vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt    ///
    cap_relative joint_dem lgdist iss_peaceyr if chal>=200& tgt>=200, robust

logit onset vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt   ///
	cap_relative joint_dem lgdist iss_peaceyr if chal>=200& tgt>=200, robust


* Table A4a: Western Hemisphere

use "SLM_Militarization_19012001.dta", clear

logit midissyr c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal   ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt  ///
	cap_relative joint_dem lgdist mid_peaceyr if terriss==1 & chal<200 & tgt<200, robust

logit midissyr c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt ///
    cap_relative joint_dem lgdist mid_peaceyr if riveriss==1 & chal<200 & tgt<200, robust

logit midissyr c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt ///
    cap_relative joint_dem lgdist mid_peaceyr if mariss==1 & chal<200 & tgt<200, robust

logit midissyr c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt ///
	cap_relative joint_dem lgdist mid_peaceyr if chal<200 & tgt<200, robust

logit midissyr vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt   ///
	cap_relative joint_dem lgdist mid_peaceyr if terriss==1 & chal<200 & tgt<200, robust

logit midissyr vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt  ///
    cap_relative joint_dem lgdist mid_peaceyr if riveriss==1 & chal<200 & tgt<200, robust

logit midissyr vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt  ///
    cap_relative joint_dem lgdist mid_peaceyr if mariss==1 & chal<200 & tgt<200, robust

logit midissyr vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt  ///
	cap_relative joint_dem lgdist mid_peaceyr if chal<200 & tgt<200, robust

* Table A4b: Europe

logit midissyr c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal   ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt  ///
	cap_relative joint_dem lgdist mid_peaceyr if terriss==1 & chal>=200 & tgt>=200, robust

*logit midissyr c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal ///
*	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt ///
*    cap_relative joint_dem lgdist mid_peaceyr if riveriss==1 & chal>=200 & tgt>=200, robust
* Not estimated

logit midissyr c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt ///
    cap_relative joint_dem lgdist mid_peaceyr if mariss==1 & chal>=200 & tgt>=200, robust

logit midissyr c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt ///
	cap_relative joint_dem lgdist mid_peaceyr if chal>=200 & tgt>=200, robust

logit midissyr vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt  ///
	cap_relative joint_dem lgdist mid_peaceyr if terriss==1 & chal>=200 & tgt>=200, robust

*logit midissyr vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt ///
*    cap_relative joint_dem lgdist mid_peaceyr if riveriss==1 & chal>=200 & tgt>=200, robust

logit midissyr vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt ///
    cap_relative joint_dem lgdist mid_peaceyr if mariss==1 & chal>=200 & tgt>=200, robust

logit midissyr vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt ///
	cap_relative joint_dem lgdist mid_peaceyr if chal>=200 & tgt>=200, robust

** Table A5a: Contiguous Territory Replications
use "cont_terr_replications.dta", clear

logit onset c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal  ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt, robust

logit onset c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal  ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt ///
	cap_relative joint_dem lgdist iss_peaceyr, robust

logit onset vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt, robust

logit onset vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt ///
	cap_relative joint_dem lgdist iss_peaceyr, robust

** Table A5b
use cont_terr_manage_replications.dta, clear

logit midissyr c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal  ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt, robust

logit midissyr c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal  ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt ///
	cap_relative joint_dem lgdist mid_peaceyr, robust

logit midissyr vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt, robust

logit midissyr vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt ///
	cap_relative joint_dem lgdist mid_peaceyr, robust
	
** Table A6: Replications
use bhrepdata_pretmp.dta, clear

* Table A6a
logit rivclm s_pre_up s_pre_down s_tmp_up s_tmp_down, robust

logit rivclm c.s_pre_up##c.s_pre_up c.s_pre_down##c.s_pre_down c.s_tmp_up##c.s_tmp_up c.s_tmp_down##c.s_tmp_down, robust

logit rivclm lnrunofft lnpopdenst lnlength rivercross treatany lnigos demaut7 relcapd s_pre_up s_pre_down s_tmp_up s_tmp_down, robust

logit rivclm lnrunofft lnpopdenst lnlength rivercross treatany lnigos demaut7 relcapd ///
c.s_pre_up##c.s_pre_up c.s_pre_down##c.s_pre_down c.s_tmp_down##c.s_tmp_down, robust

** Table A6b

heckprob attanyp lnrunofft lnpopdenst lnlength rivercross icowsal treatclm lnigos demaut7 ///
relcapc s_pre_chal s_pre_tgt,  ///
select(rivclm=lnrunofft lnpopdenst lnlength rivercross treatany lnigos demaut7 relcapd ///
s_pre_up s_pre_down) robust

heckprob attanyp lnrunofft lnpopdenst lnlength rivercross icowsal treatclm lnigos demaut7 ///
relcapc c.s_pre_chal##c.s_pre_chal c.s_pre_tgt##c.s_pre_tgt,  ///
select(rivclm=lnrunofft lnpopdenst lnlength rivercross treatany lnigos demaut7 relcapd ///
c.s_pre_up##c.s_pre_up c.s_pre_down##c.s_pre_down) robust

** Table A6c

logit midissyr s_pre_chal s_pre_tgt s_tmp_chal s_tmp_tgt, robust

logit midissyr c.s_pre_chal##c.s_pre_chal c.s_pre_tgt##c.s_pre_tgt c.s_tmp_chal##c.s_tmp_chal c.s_tmp_tgt##c.s_tmp_tgt, robust

logit midissyr lnrunofft lnpopdenst lnlength rivercross icowsal treatclm lnigos demaut7 relcapc ///
s_pre_chal s_pre_tgt s_tmp_chal s_tmp_tgt, robust

logit midissyr lnrunofft lnpopdenst lnlength rivercross icowsal treatclm lnigos demaut7 relcapc ///
c.s_pre_chal##c.s_pre_chal c.s_pre_tgt##c.s_pre_tgt c.s_tmp_chal##c.s_tmp_chal c.s_tmp_tgt##c.s_tmp_tgt, robust


** Table A7 Replications
use "dmrepdata_pretmp.dta", clear

* Table A7a
logit mariss3 s_pre_1 s_pre_2 s_tmp_1 s_tmp_2, robust

logit mariss3 c.s_pre_1##c.s_pre_1 c.s_pre_2##c.s_pre_2 c.s_tmp_1##c.s_tmp_1 c.s_tmp_2##c.s_tmp_2, robust

logit mariss3 dyaddem mixeddyad relcapenergy majpow cwmid americas europe asia middleeast s_pre_1 s_pre_2 s_tmp_1 s_tmp_2 year yearsq, robust

logit mariss3 dyaddem mixeddyad relcapenergy majpow cwmid americas europe asia middleeast ///
c.s_pre_1##c.s_pre_1 c.s_pre_2##c.s_pre_2 c.s_tmp_1##c.s_tmp_1 c.s_tmp_2##c.s_tmp_2 year yearsq, robust


* Table A7b
logit cwmid s_pre_1 s_pre_2 s_tmp_1 s_tmp_2, robust

logit cwmid c.s_pre_1##c.s_pre_1 c.s_pre_2##c.s_pre_2 c.s_tmp_1##c.s_tmp_1 c.s_tmp_2##c.s_tmp_2, robust

logit cwmid dyaddem mixeddyad relcapenergy majpow americas europe asia middleeast s_pre_1 s_pre_2 s_tmp_1 s_tmp_2 year yearsq, robust

logit cwmid dyaddem mixeddyad relcapenergy majpow americas europe asia middleeast ///
c.s_pre_1##c.s_pre_1 c.s_pre_2##c.s_pre_2 c.s_tmp_1##c.s_tmp_1 c.s_tmp_2##c.s_tmp_2 year yearsq, robust


** Table A8a
use "SLM_Onset_19012001.dta", clear

logit terriss c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal vol_pre_chal vol_tmp_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt vol_pre_tgt vol_tmp_tgt   ///
	cap_relative joint_dem lgdist iss_peaceyr, robust

logit riveriss c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal vol_pre_chal vol_tmp_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt vol_pre_tgt vol_tmp_tgt  ///
    cap_relative joint_dem lgdist iss_peaceyr, robust

logit mariss c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal vol_pre_chal vol_tmp_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt vol_pre_tgt vol_tmp_tgt  ///
    cap_relative joint_dem lgdist iss_peaceyr, robust
 
logit onset c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal vol_pre_chal vol_tmp_chal  ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt vol_pre_tgt vol_tmp_tgt  ///
	cap_relative joint_dem lgdist iss_peaceyr, robust

** Table A8b
use "SLM_Militarization_19012001.dta", clear

logit midissyr c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal vol_pre_chal vol_tmp_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt vol_pre_tgt vol_tmp_tgt  ///
	cap_relative joint_dem lgdist mid_peaceyr if terriss==1, robust

logit midissyr c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal vol_pre_chal vol_tmp_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt vol_pre_tgt vol_tmp_tgt ///
    cap_relative joint_dem lgdist mid_peaceyr if riveriss==1, robust

logit midissyr c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal vol_pre_chal vol_tmp_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt vol_pre_tgt vol_tmp_tgt ///
    cap_relative joint_dem lgdist mid_peaceyr if mariss==1, robust

logit midissyr c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal vol_pre_chal vol_tmp_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt vol_pre_tgt vol_tmp_tgt ///
	cap_relative joint_dem lgdist mid_peaceyr, robust
	
** Table A9a
use "SLM_Onset_19012001.dta" 

logit terriss c.s_tmpn_chal##c.s_tmpn_chal c.s_precn_chal##c.s_precn_chal  ///
	c.s_tmpn_tgt##c.s_tmpn_tgt c.s_precn_tgt##c.s_precn_tgt   ///
	cap_relative joint_dem lgdist iss_peaceyr, robust

logit riveriss c.s_tmpn_chal##c.s_tmpn_chal c.s_precn_chal##c.s_precn_chal  ///
	c.s_tmpn_tgt##c.s_tmpn_tgt c.s_precn_tgt##c.s_precn_tgt   ///
    cap_relative joint_dem lgdist iss_peaceyr, robust

logit mariss c.s_tmpn_chal##c.s_tmpn_chal c.s_precn_chal##c.s_precn_chal  ///
	c.s_tmpn_tgt##c.s_tmpn_tgt c.s_precn_tgt##c.s_precn_tgt   ///
    cap_relative joint_dem lgdist iss_peaceyr, robust

logit onset c.s_tmpn_chal##c.s_tmpn_chal c.s_precn_chal##c.s_precn_chal  ///
	c.s_tmpn_tgt##c.s_tmpn_tgt c.s_precn_tgt##c.s_precn_tgt   ///
	cap_relative joint_dem lgdist iss_peaceyr, robust

logit terriss vol_precn_chal vol_tmpn_chal vol_precn_tgt vol_tmpn_tgt   ///
	cap_relative joint_dem lgdist iss_peaceyr, robust

logit riveriss vol_precn_chal vol_tmpn_chal vol_precn_tgt vol_tmpn_tgt   ///
    cap_relative joint_dem lgdist iss_peaceyr, robust

logit mariss vol_precn_chal vol_tmpn_chal vol_precn_tgt vol_tmpn_tgt   ///
    cap_relative joint_dem lgdist iss_peaceyr, robust
 
logit onset vol_precn_chal vol_tmpn_chal vol_precn_tgt vol_tmpn_tgt   ///
	cap_relative joint_dem lgdist iss_peaceyr, robust


** Table A9b
use "SLM_Militarization_19012001.dta", clear

logit midissyr c.s_tmpn_chal##c.s_tmpn_chal c.s_precn_chal##c.s_precn_chal   ///
	c.s_tmpn_tgt##c.s_tmpn_tgt c.s_precn_tgt##c.s_precn_tgt  ///
	cap_relative joint_dem lgdist mid_peaceyr if (terriss==1), robust

logit midissyr c.s_tmpn_chal##c.s_tmpn_chal c.s_precn_chal##c.s_precn_chal   ///
	c.s_tmpn_tgt##c.s_tmpn_tgt c.s_precn_tgt##c.s_precn_tgt  ///
    cap_relative joint_dem lgdist mid_peaceyr if (riveriss==1), robust

logit midissyr c.s_tmpn_chal##c.s_tmpn_chal c.s_precn_chal##c.s_precn_chal   ///
	c.s_tmpn_tgt##c.s_tmpn_tgt c.s_precn_tgt##c.s_precn_tgt  ///
    cap_relative joint_dem lgdist mid_peaceyr if (mariss==1), robust

logit midissyr c.s_tmpn_chal##c.s_tmpn_chal c.s_precn_chal##c.s_precn_chal   ///
	c.s_tmpn_tgt##c.s_tmpn_tgt c.s_precn_tgt##c.s_precn_tgt  ///
	cap_relative joint_dem lgdist mid_peaceyr, robust

logit midissyr vol_precn_chal vol_tmpn_chal vol_precn_tgt vol_tmpn_tgt  ///
	cap_relative joint_dem lgdist mid_peaceyr if (terriss==1), robust

logit midissyr vol_precn_chal vol_tmpn_chal vol_precn_tgt vol_tmpn_tgt  ///
    cap_relative joint_dem lgdist mid_peaceyr if (riveriss==1), robust

logit midissyr vol_precn_chal vol_tmpn_chal vol_precn_tgt vol_tmpn_tgt  ///
    cap_relative joint_dem lgdist mid_peaceyr if (mariss==1), robust

logit midissyr vol_precn_chal vol_tmpn_chal vol_precn_tgt vol_tmpn_tgt  ///
	cap_relative joint_dem lgdist mid_peaceyr, robust

	
** Table A10a
logit terriss c.s_tmp30_chal##c.s_tmp30_chal c.s_prec30_chal##c.s_prec30_chal  ///
	c.s_tmp30_tgt##c.s_tmp30_tgt c.s_prec30_tgt##c.s_prec30_tgt   ///
	cap_relative joint_dem lgdist iss_peaceyr, robust

logit riveriss c.s_tmp30_chal##c.s_tmp30_chal c.s_prec30_chal##c.s_prec30_chal  ///
	c.s_tmp30_tgt##c.s_tmp30_tgt c.s_prec30_tgt##c.s_prec30_tgt   ///
    cap_relative joint_dem lgdist iss_peaceyr, robust

logit mariss c.s_tmp30_chal##c.s_tmp30_chal c.s_prec30_chal##c.s_prec30_chal  ///
	c.s_tmp30_tgt##c.s_tmp30_tgt c.s_prec30_tgt##c.s_prec30_tgt   ///
    cap_relative joint_dem lgdist iss_peaceyr, robust

logit onset c.s_tmp30_chal##c.s_tmp30_chal c.s_prec30_chal##c.s_prec30_chal  ///
	c.s_tmp30_tgt##c.s_tmp30_tgt c.s_prec30_tgt##c.s_prec30_tgt   ///
	cap_relative joint_dem lgdist iss_peaceyr, robust

logit terriss  vol_prec30_chal vol_tmp30_chal vol_prec30_tgt vol_tmp30_tgt   ///
	cap_relative joint_dem lgdist iss_peaceyr, robust

logit riveriss vol_prec30_chal vol_tmp30_chal vol_prec30_tgt vol_tmp30_tgt   ///
    cap_relative joint_dem lgdist iss_peaceyr, robust

logit mariss vol_prec30_chal vol_tmp30_chal vol_prec30_tgt vol_tmp30_tgt   ///
    cap_relative joint_dem lgdist iss_peaceyr, robust
 
logit onset vol_prec30_chal vol_tmp30_chal vol_prec30_tgt vol_tmp30_tgt   ///
	cap_relative joint_dem lgdist iss_peaceyr, robust

	
** Table A10b
logit midissyr c.s_tmp30_chal##c.s_tmp30_chal c.s_prec30_chal##c.s_prec30_chal   ///
	c.s_tmp30_tgt##c.s_tmp30_tgt c.s_prec30_tgt##c.s_prec30_tgt  ///
	cap_relative joint_dem lgdist mid_peaceyr if terriss==1, robust

logit midissyr c.s_tmp30_chal##c.s_tmp30_chal c.s_prec30_chal##c.s_prec30_chal   ///
	c.s_tmp30_tgt##c.s_tmp30_tgt c.s_prec30_tgt##c.s_prec30_tgt  ///
    cap_relative joint_dem lgdist mid_peaceyr if riveriss==1, robust

logit midissyr c.s_tmp30_chal##c.s_tmp30_chal c.s_prec30_chal##c.s_prec30_chal   ///
	c.s_tmp30_tgt##c.s_tmp30_tgt c.s_prec30_tgt##c.s_prec30_tgt  ///
    cap_relative joint_dem lgdist mid_peaceyr if mariss==1, robust

logit midissyr c.s_tmp30_chal##c.s_tmp30_chal c.s_prec30_chal##c.s_prec30_chal   ///
	c.s_tmp30_tgt##c.s_tmp30_tgt c.s_prec30_tgt##c.s_prec30_tgt  ///
	cap_relative joint_dem lgdist mid_peaceyr, robust

logit midissyr vol_prec30_chal vol_tmp30_chal vol_prec30_tgt vol_tmp30_tgt  ///
	cap_relative joint_dem lgdist mid_peaceyr if terriss==1, robust

logit midissyr vol_prec30_chal vol_tmp30_chal vol_prec30_tgt vol_tmp30_tgt  ///
    cap_relative joint_dem lgdist mid_peaceyr if riveriss==1, robust

logit midissyr vol_prec30_chal vol_tmp30_chal vol_prec30_tgt vol_tmp30_tgt  ///
    cap_relative joint_dem lgdist mid_peaceyr if mariss==1, robust

logit midissyr vol_prec30_chal vol_tmp30_chal vol_prec30_tgt vol_tmp30_tgt  ///
	cap_relative joint_dem lgdist mid_peaceyr, robust
