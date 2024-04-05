clear all
set more off
capture log close

*---- Set Directory ----*
global rootDir = ""
global dataDir = "Data/Intermediate"
global logDir = "Analysis/logSTATA"
global resultDir = "Result/resultSTATA"

*---- Log File Path ----*
cd "$rootDir/$logDir"
//log using 12_GDAT_imputeCompletionYear_test, replace



*******************************************************************************
/*				   		 GLOBAL DAMS TRACKER (GDAT)        					 */
*******************************************************************************

/* 

File Name:    	12_GDAT_imputeCompletionYear_test.do

By: 	    	Alice Tianbo Zhang      	    

First Edited:  	2021/05/20
This Version: 	

Purpose:	

Data Used:     

Data Created:   

Program Used:  

*/

*******************************************************************************
/*                       	  Load Training Data	 	 		   		     */
*******************************************************************************

import delimited "$rootDir/$dataDir/NoCompletionYear/dams_test_all.csv", clear 

reshape long y, i(feature_id) j(year) string
rename y pixelCount

* gen waterClass = substr(year,-1,1) 
* reshape wide pixelCount, i(feature_id) j(waterClass) string


capture drop waterClass*
forvalues i=1/3{
	gen waterClass_`i' = .
	replace waterClass_`i' = pixelCount if regexm(year, "_`i'")
}
replace year = substr(year,1,4) 

collapse (firstnm) waterClass_* actual_yr, by(feature_id year) 


*******************************************************************************
/*                      	 Impute Water Pixels	 	 		   		     */
*******************************************************************************
destring year, replace
sort feature_id year
xtset feature_id year

replace waterClass_1 = . if waterClass_1 == 0 & waterClass_2 == 0 & waterClass_3 == 0 
replace waterClass_2 = . if waterClass_1 == . & waterClass_2 == 0 & waterClass_3 == 0 
replace waterClass_3 = . if waterClass_1 == . & waterClass_2 == . & waterClass_3 == 0 


forvalues i=1/3{
	
	capture drop impute_waterClass_`i'
	capture drop n_missing_`i'
	
	// Impute water pixels
	gen impute_waterClass_`i' = waterClass_`i'
	replace impute_waterClass_`i' = l1.impute_waterClass_`i' if missing(impute_waterClass_`i')
	
	// Count missing values(years)
	egen n_missing_`i' = count(impute_waterClass_`i'), by(feature_id)
	replace n_missing_`i' = 35 - n_missing_`i' 
		
}
count if n_missing_1 != n_missing_2 | n_missing_1 != n_missing_3 | n_missing_2 != n_missing_3 // missing years consistent across water class
unique feature_id if n_missing_1 > 5 // 1017 dams with more than 5 years pixel count missing 
drop if n_missing_1 > 5 // 7324 dams left
gen impute_waterClass_23 = impute_waterClass_2 + impute_waterClass_3 

order impute_waterClass_*, before(n_missing_1)

*******************************************************************************
/*                       Calculate Annual Changes	 	 		   		     */
*******************************************************************************

forvalues i=1/3{

	capture drop diff_waterClass_`i'
	capture drop pdiff_waterClass_`i'
	bysort feature_id (year): gen diff_waterClass_`i' = impute_waterClass_`i' - l1.impute_waterClass_`i'
	bysort feature_id (year): gen pdiff_waterClass_`i' = (impute_waterClass_`i' - l1.impute_waterClass_`i') / l1.impute_waterClass_`i'
	
}
order diff_waterClass_*, before(pdiff_waterClass_1)


*******************************************************************************
/*                       Impute Completion Year	 	 		   		  	     */
*******************************************************************************

*----------------- Method: Maximum Change in Water Pixels --------------*

** Seperate each water class

forvalues i=1/3{
	bysort feature_id: egen max_dWater`i' = max(diff_waterClass_`i')
	by feature_id: egen year_max`i' = min(cond(diff_waterClass_`i' == max_dWater`i', year, .))
}

capture drop diff_waterClass_23
capture drop max_dWater23 
capture drop d_correctYear23
capture drop year_max23

** Combine classes 2 & 3 
gen diff_waterClass_23 = diff_waterClass_2 + diff_waterClass_3
bysort feature_id: egen max_dWater23 = max(diff_waterClass_23)
by feature_id: egen year_max23 = min(cond(diff_waterClass_23 == max_dWater23, year, .))


*----------------- Method: Structural Break Test --------------*
** Seperate each water class
sort feature_id 
capture drop group
egen group = group(feature_id)
//gen yfitted = . 

forvalues c = 1/3 {
	capture drop year_sb`c'
	gen year_sb`c' = . 
	sum group, meanonly 
	
	forvalues g = 1/`r(max)'{
		capture sum feature_id if group == `g'
		di "Dam feature ID `r(min)' | group `g'"
		//tab feature_id if group == `g'
		capture reg impute_waterClass_`c' year if group == `g', vce(robust) 
		//predict yfit 
		//replace yfitted = yfit if group == `g'
		//drop yfit
		
		capture estat sbsingle 
			if _rc == 0 {
				if `r(p)' <= 0.01{
					di "Detect structural break in `r(breakdate)'"
					replace year_sb`c' = `r(breakdate)' if group == `g'
				}
			}
		/* Known break year
		forvalues y = 1988/2014{ 
			// Need to exclude beginning and end of time series, otherwise "insufficient observations at the specified break date" 
			capture estat sbknown, break(`y')
			if _rc == 0 {
				if `r(p)' <= 0.01{
					di "Detect structural break in `y'"
					replace d_sb_waterClass`c' = 1 if year == `y'
				}
			}
		}
		*/
	}
}
			
** Combine classes 2 & 3 
gen year_sb23 = . 
sum group, meanonly 
forvalues g = 1/`r(max)'{
		capture sum feature_id if group == `g'
		di "Dam feature ID `r(min)' | group `g'"
		capture reg impute_waterClass_23 year if group == `g', vce(robust) 
		//predict yfit 
		//replace yfitted = yfit if group == `g'
		//drop yfit
		
		capture estat sbsingle 
			if _rc == 0 {
				if `r(p)' <= 0.01{
					di "Detect structural break in `r(breakdate)'"
					replace year_sb23 = `r(breakdate)' if group == `g'
				}
			}
}

save "$rootDir/$dataDir/NoCompletionYear/dams_test_impute.dta", replace


*******************************************************************************
/*                       Check Imputation Results	 	 		   	 	     */
*******************************************************************************

use "$rootDir/$dataDir/NoCompletionYear/dams_test_impute.dta", clear

** Collapse & reshape data

* Collapse by dam feature ID 
collapse (firstnm) year_sb* year_max*, by(feature_id) 

* Reshape into long format 
reshape long year_, i(feature_id) j(method) string

** Create mode of imputed year
capture drop mode* 
capture drop minmode*

bysort feature_id: egen minmode_yr = mode(year_), minmode
bysort feature_id: egen mode_yr = mode(year_)

** Check results
collapse (firstnm) minmode_yr mode_yr, by(feature_id)

hist minmode_yr, percent width(2) xtitle("Year") addlabel addlabopts(mlabsize(vsmall) yvarformat(%4.1f)) ///
						normal normopts(lcolor(blue)) ///
						xtick(1985(3)2018) xlabel(1985(3)2018, nogrid) ///
						ylabel(, nogrid) ///
						fcolor(gs12) lwidth(vthin) ///
						title("Distribution of Imputed Completion Year", size(medium)) ///
						scheme(myplain) graphregion(color(white))  name("hist_imputeYear", replace) 
	
cd "$rootDir/$resultDir"
graph export hist_impute_mode_yr.pdf, name(hist_imputeYear) replace

** Save results
save "$rootDir/$dataDir/NoCompletionYear/test_impute_yr.dta", replace
 
 
** Merge with training data
gen actual_yr = minmode_yr
gen d_impute_yr = 1
append using "$rootDir/$dataDir/WithCompletionYear/training_impute_yr.dta"
replace d_impute_yr = 0 if d_impute_yr == .
sort feature_id

save "$rootDir/$dataDir/dams_impute_completeYr.dta"


log close


