*** SLM, "Climate Bones of Contention"

use "SLM_Onset_19012001.dta", clear 

** Figure 5, 6 & 9: All Types of Issue Onset 
* Figure 5
qui logit onset c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt  ///
	cap_relative joint_dem lgdist iss_peaceyr, robust
	
margins, at(s_pre_chal=(-2.5(0.5)2.5)) predict(xb) saving(onset_all_pre)
 
margins, at(s_tmp_chal=(-2.5(0.5)2.5)) predict(xb) saving(onset_all_tmp)

use onset_all_pre, clear
foreach v of varlist _margin _ci_lb _ci_ub {
    replace `v' = invlogit(`v')
}

twoway  ///
    (rcap _ci_lb _ci_ub _at2, sort pstyle(ci) color(gs7) lw(median)) ///
	(connected _margin _at2, sort color(black)), ///
    xlabel(-2.5(.5)2.5, labsize(small)) ///
	ylabel(0(.002)0.0065, labsize(small)) ///
	xtitle("Challenger's Precipitation") ///
	ytitle("Pr(Issue Onset)" "") ///
	graphregion(color(white)) ///
	title(" ") ///
	legend(off) ///
	note("Note: 95% Confidence Intervals reported.", size(small)) ///
	saving(onset_all_pre.gph, replace)

* Figure 6
use onset_all_tmp, clear
foreach v of varlist _margin _ci_lb _ci_ub {
    replace `v' = invlogit(`v')
}

twoway  ///
    (rcap _ci_lb _ci_ub _at1, sort pstyle(ci) color(gs7) lw(median)) ///
	(connected _margin _at1, sort color(black)), ///
    xlabel(-2.5(.5)2.5, labsize(small)) ///
	ylabel(0(.001)0.005, labsize(small)) ///
	xtitle("Challenger's Temperature") ///
	ytitle("Pr(Issue Onset)" "") ///
	graphregion(color(white)) ///
	title(" ") ///
	legend(off) ///
	note("Note: 95% Confidence Intervals reported.", size(small)) ///
	saving(onset_all_tmp.gph, replace)


 
* Figure 9: Volatility 	
use "SLM_Onset_19012001.dta", clear 

qui logit onset vol_pre_chal vol_tmp_chal  vol_pre_tgt vol_tmp_tgt  ///
	cap_relative joint_dem lgdist iss_peaceyr, robust

margins, at(vol_tmp_chal=(-2.5(0.5)2.5)) predict(xb) saving(onset_all_vol)

use onset_all_vol, clear
foreach v of varlist _margin _ci_lb _ci_ub {
    replace `v' = invlogit(`v')
}

twoway  ///
    (rcap _ci_lb _ci_ub _at2, sort pstyle(ci) color(gs7) lw(median)) ///
	(connected _margin _at2, sort color(black)), ///
    xlabel(-2.5(.5)2.5, labsize(small)) ///
	ylabel(0(.001)0.005, labsize(small)) ///
	xtitle("Challenger's Temperature Volatility") ///
	ytitle("Pr(Issue Onset)" "") ///
	graphregion(color(white)) ///
	title(" ") ///
	legend(off) ///
	note("Note: 95% Confidence Intervals reported.", size(small)) ///
	saving(onset_all_vol.gph, replace)
	

** Figure 7, 8, & 10: Militarization
	
use "SLM_Militarization_19012001.dta", clear

* Figure 7: All
qui logit midissyr c.s_tmp_chal##c.s_tmp_chal c.s_pre_chal##c.s_pre_chal ///
	c.s_tmp_tgt##c.s_tmp_tgt c.s_pre_tgt##c.s_pre_tgt ///
	cap_relative joint_dem lgdist mid_peaceyr, robust

margins, at(s_pre_chal=(-2.5(0.5)2.5)) predict(xb) saving(mil_all_chal)

margins, at(s_pre_tgt=(-2.5(0.5)2.5)) predict(xb) saving(mil_all_tgt)

use mil_all_chal, clear
foreach v of varlist _margin _ci_lb _ci_ub {
    replace `v' = invlogit(`v')
}
twoway  ///
    (rcap _ci_lb _ci_ub _at2, sort pstyle(ci) color(gs7) lw(median)) ///
	(connected _margin _at2, sort color(black)), ///
    xlabel(-2.5(.5)2.5, labsize(small)) ///
	ylabel(0(.03)0.1, labsize(small)) ///
	xtitle("Challenger's Precipitation") ///
	ytitle("Pr(Issue Militarization)" "") ///
	graphregion(color(white)) ///
	title(" ") ///
	legend(off) ///
	note("Note: 95% Confidence Intervals reported.", size(small)) ///
	saving(mil_all_chal.gph, replace)

* Figure 8
use mil_all_tgt, clear
foreach v of varlist _margin _ci_lb _ci_ub {
    replace `v' = invlogit(`v')
}
twoway  ///
    (rcap _ci_lb _ci_ub _at4, sort pstyle(ci) color(gs7) lw(median)) ///
	(connected _margin _at4, sort color(black)), ///
    xlabel(-2.5(.5)2.5, labsize(small)) ///
	ylabel(0(.02).06, labsize(small)) ///
	xtitle("Target's Precipitation") ///
	ytitle("Pr(Issue Militarization)" "") ///
	graphregion(color(white)) ///
	title(" ") ///
	legend(off) ///
	note("Note: 95% Confidence Intervals reported.", size(small)) ///
	saving(mil_all_tgt.gph, replace)

logit midissyr vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt ///
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
	

use "SLM_Militarization_19012001.dta", clear

qui logit midissyr vol_pre_chal vol_tmp_chal vol_pre_tgt vol_tmp_tgt ///
	cap_relative joint_dem lgdist mid_peaceyr, robust
	
margins, at(vol_tmp_chal=(-2.5(0.5)2.5)) predict(xb) saving(mil_all_vol, replace)

use mil_all_vol, clear
foreach v of varlist _margin _ci_lb _ci_ub {
    replace `v' = invlogit(`v')
}
twoway  ///
    (rcap _ci_lb _ci_ub _at2, sort pstyle(ci) color(gs7) lw(median)) ///
	(connected _margin _at2, sort color(black)), ///
    xlabel(-2.5(.5)2.5, labsize(small)) ///
	ylabel(0(.01)0.05, labsize(small)) ///
	xtitle("Challenger's Temperature Volatility") ///
	ytitle("Pr(Issue Militarization)" "") ///
	graphregion(color(white)) ///
	title(" ") ///
	legend(off) ///
	note("Note: 95% Confidence Intervals reported.", size(small)) ///
	saving(mil_all_vol.gph, replace)
	