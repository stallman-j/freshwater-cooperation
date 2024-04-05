clear all
set more off
capture log close

*---- Set Directory ----*
global rootDir = ""
global dataDir = "Data/Intermediate"
global dataDir2 = "Data/Final"
global logDir = "Analysis/logSTATA"
global resultDir = "Result/resultSTATA"

*---- Log File Path ----*
cd "$rootDir/$logDir"
//log using 13_GDAT_calculateChange, replace



*******************************************************************************
/*				   		 GLOBAL DAMS TRACKER (GDAT)        					 */
*******************************************************************************

/* 

File Name:    	13_GDAT_calculateChange.do

By: 	    	Alice Tianbo Zhang      	    

First Edited:  	2021/06/07
This Version: 	

Purpose:	

Data Used:     

Data Created:   

Program Used:  

*/

*******************************************************************************
/*                     CALCULATE CATCHMENT AREA CHANGES	 	 		   		 */
*******************************************************************************

*-------------------------- Merge training & test data -----------------------*
cd "$rootDir/$dataDir"
import delimited "./NoCompletionYear/dams_test_all.csv", clear 
tempfile test
save `test'

import delimited "./WithCompletionYear/dams_training_all.csv", clear
append using `test'

*------------------------ Merge Imputed Completion Year ----------------------*
merge 1:1 feature_id using dams_impute_completeYr.dta, update // 1743 dams not merged, more than 5 years pixel count missing
drop if _merge == 1 

** Reshape into long 
reshape long y, i(feature_id) j(year) string
rename y pixelCount

capture drop waterClass*
forvalues i=1/3{
	gen waterClass_`i' = .
	replace waterClass_`i' = pixelCount if regexm(year, "_`i'")
}
replace year = substr(year,1,4) 

collapse (firstnm) waterClass_* actual_yr d_impute_yr, by(feature_id year) 


** Impute water pixels
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
gen impute_waterClass_23 = impute_waterClass_2 + impute_waterClass_3 
gen impute_waterClass_all = impute_waterClass_1 + impute_waterClass_23


*----------------- Calculate Summay Statistics of Water Coverage -------------*
capture drop pre*
capture drop post*

foreach i in 1 2 3 23 { 
	bysort feature_id: egen pre_min_waterClass`i' = min(impute_waterClass_`i') if year <= actual_yr
	bysort feature_id: egen post_min_waterClass`i' = min(impute_waterClass_`i') if year > actual_yr

	bysort feature_id: egen pre_max_waterClass`i' = max(impute_waterClass_`i') if year <= actual_yr
	bysort feature_id: egen post_max_waterClass`i' = max(impute_waterClass_`i') if year > actual_yr

	bysort feature_id: egen pre_median_waterClass`i' = median(impute_waterClass_`i') if year <= actual_yr
	bysort feature_id: egen post_median_waterClass`i' = median(impute_waterClass_`i') if year > actual_yr

	bysort feature_id: egen pre_mean_waterClass`i' = mean(impute_waterClass_`i') if year <= actual_yr
	bysort feature_id: egen post_mean_waterClass`i' = mean(impute_waterClass_`i') if year > actual_yr

	bysort feature_id: egen pre_mode_waterClass`i' = mode(impute_waterClass_`i') if year <= actual_yr, minmode
	bysort feature_id: egen post_mode_waterClass`i' = mode(impute_waterClass_`i') if year > actual_yr, minmode

}

foreach stat in min max median mean mode{
capture drop `stat'
	foreach i in 1 2 3 23 { 
		display "Calculate `stat' water coverage for pre- and post-completion"
		gen `stat'_completeYr_waterClass`i' = .
		replace `stat'_completeYr_waterClass`i' = pre_`stat'_waterClass`i' if year <= actual_yr
		replace `stat'_completeYr_waterClass`i' = post_`stat'_waterClass`i' if year > actual_yr
	
		display "Number of unique dams with missing observations" 
		unique feature_id if `stat'_completeYr_waterClass`i' == . // 347 dams with missing observations
	}
}

** Merge w/t GDAT attributes
rename feature_id Feature_ID
cd "$rootDir/$dataDir2/v0"
merge m:1 Feature_ID using GlobalDam_v0_abbVarName.dta
destring Year_Fin, gen(Year_Fin_num) force 
unique Feature_ID if Year_Fin_num >= 1985 & _merge == 2
drop if _merge == 2 // 23,057 dams dropped; 6706 dams completed on or after 1985

** Save data
cd "$rootDir/$dataDir"
save "dams_training_test_all.dta", replace


*----------------- Save surface water coverage summary stat -----------------*
use "dams_training_test_all.dta", clear
keep Feature_ID year actual_yr d_impute_yr min_completeYr_waterClass1-mode_completeYr_waterClass23

** Reshape into long 
unab vars: *1
local stubs: subinstr local vars "1" "", all
di "`stubs'"

reshape long `stubs', i(Feature_ID year) j(waterClass)

** Collapse by feature_id
capture drop d_post
gen d_post = (year > actual_yr)
collapse (firstnm) *_completeYr_* actual_yr d_impute_yr, by(Feature_ID d_post waterClass) 

rename *_completeYr_* *_completeYr


** Merge w/t GDAT attributes 
cd "$rootDir/$dataDir2/v0"
merge m:1 Feature_ID using GlobalDam_v0_abbVarName.dta
destring Year_Fin, gen(Year_Fin_num) force 
unique Feature_ID if Year_Fin_num >= 1985 & _merge == 2
drop if _merge == 2 // 23,057 dams dropped; 6706 dams completed on or after 1985

** Save data
cd "$rootDir/$dataDir"
save "dams_waterClass_byCompleteYr.dta", replace



*--------------- Plot Results: Bar Graph (Global & By Continent) ------------*
cd "$rootDir/$dataDir"
use "dams_waterClass_byCompleteYr.dta", clear

collapse (sum) *_completeYr (firstnm) actual_yr d_impute_yr, by(waterClass d_post Continent)

** Recode continent for plot order
gen continent_num = .
replace continent_num = 1 if Continent == "North America"
replace continent_num = 2 if Continent == "Asia"
replace continent_num = 3 if Continent == "Europe"
replace continent_num = 4 if Continent == "South America"
replace continent_num = 5 if Continent == "Africa"
replace continent_num = 6 if Continent == "Oceania"
label define continentl 1 "North America" 2 "Asia" 3 "Europe" 4 "South America" 5 "Africa" 6 "Oceania"
label values continent_num continentl

** Convert pixel count to area (km^2)
foreach var of varlist *_completeYr {
	gen `var'_thkm2 = `var' * 900 / 1000000 / 1000
}

drop if waterClass == 23
gen d_water = inlist(waterClass,2,3)

* label define periodl 0 "Pre-dam" 1 "Post-dam"
label define periodl 0 "Pre-dam" 1 "Post-dam"
label values d_post periodl

label define waterl 0 "No water" 1 "Water"
label values d_water waterl

label define water_typel 1 "No water" 2 "Seasonal water" 3 "Permanent water"
label values waterClass water_typel

foreach stat in min max median mean mode{
	gen `stat'_completeYr_mln = `stat'_completeYr / 1000000
}

** Global
graph bar (sum) median_completeYr_mln, asyvars stack over(waterClass) over(d_post, gap(*2)) outergap(100) ///
						ytitle("Pixel Count (10{superscript:6})") ///
						legend(off) ///
						/* legend(col(1) position(3) order(3 2 1) rowgap(*3)) */ ///
						ysc(r(0 300) lcolor(gs10)) ylabel(0(50)300, nogrid) xsize(17) ysize(12) ///
						graphregion(color(white)) /// 
						bar(1, bcolor(black%30)) bar(2, bcolor(midblue%30)) bar(3, bcolor(midblue)) ///
						scheme(myplain) name(bar_pixelChange_global, replace)
						
cd "$rootDir/Result/resultSTATA" 
graph export bar_pixelChange_global.png, name(bar_pixelChange_global) replace

graph bar (sum) median_completeYr_thkm2, asyvars stack over(waterClass) over(d_post, gap(*2)) outergap(100) ///
						ytitle("Area (1000 km{superscript:2})") ///
						legend(off) /// 
						/* legend(col(1) position(3) order(3 2 1) rowgap(*3)) */ /// 
						ysc(r(0 250) lcolor(gs10)) ylabel(0(50)250, nogrid) xsize(17) ysize(12) ///
						graphregion(color(white)) ///
						bar(1, bcolor(black%30)) bar(2, bcolor(midblue%30)) bar(3, bcolor(midblue)) ///
						scheme(myplain) name(bar_areaChange_global, replace)
						
graph export bar_areaChange_global.png, name(bar_areaChange_global) replace

** By Continent
graph bar (sum) median_completeYr_mln, asyvars stack over(waterClass) over(d_post, gap(*1)) outergap(120) by(continent_num, note("") imargin(b+3)) ///
						ytitle("Pixel Count (10{superscript:6})") legend(row(1) position(4) order(3 2 1) colgap(*2)) ///
						ysc(r(0 100) lcolor(gs10)) ylabel(0(20)100, nogrid) xsize(17) ysize(12) ///
						graphregion(color(white)) /// 
						bar(1, bcolor(black%30)) bar(2, bcolor(midblue%30)) bar(3, bcolor(midblue)) ///
						scheme(myplain) name(bar_pixelChange_byContinent, replace)

graph export bar_pixelChange_byContinent.png, name(bar_pixelChange_byContinent) replace

graph bar (sum) median_completeYr_thkm2, asyvars stack over(waterClass) over(d_post, gap(*1)) outergap(120) by(continent_num, note("") imargin(b+3)) ///
						ytitle("") ///
						/* ytitle("Area (1000 km{superscript:2})") */ ///
						legend(row(1) position(4) order(3 2 1) colgap(*2)) /// 
						ysc(r(0 80) lcolor(gs10)) ylabel(0(20)80, nogrid) xsize(17) ysize(12) ///
						graphregion(color(white)) ///
						bar(1, bcolor(black%30)) bar(2, bcolor(midblue%30)) bar(3, bcolor(midblue)) ///
						scheme(myplain) name(bar_areaChange_byContinent, replace)
						
graph export bar_areaChange_byContinent.png, name(bar_areaChange_byContinent) replace

				
						
*------------------------- Plot Results: Time Series ------------------------*
cd "$rootDir/$dataDir"
use "dams_training_test_all.dta", clear

** Collapse by year
tab Continent, gen(d_continent)

foreach var of varlist impute_waterClass_* {
	gen `var'_af = `var' * d_continent1
	gen `var'_as = `var' * d_continent2
	gen `var'_eu = `var' * d_continent3
	gen `var'_na = `var' * d_continent4
	gen `var'_oc = `var' * d_continent5
	gen `var'_sa = `var' * d_continent6	
}

collapse (sum) impute_waterClass_*, by(year)

** Convert pixel count to area (km^2)
foreach var of varlist impute_waterClass_* {
	gen `var'_thkm2 = `var' * 900 / 1000000 / 1000
}

** Line graph overlay all continents
graph twoway line impute_waterClass_23_na_thkm2 impute_waterClass_23_as_thkm2 impute_waterClass_23_eu_thkm2 ///
			 impute_waterClass_23_sa_thkm2 impute_waterClass_23_af_thkm2 year, ///
			 lcolor(midblue%70 cranberry%70 midgreen%70 purple%70 gold%70) ///
			 lpattern(solid solid solid solid solid) lwidth(*1.5 *1.5 *1.5 *1.5 *1.5) ///
			 msymbol(none none none none none) ///
		graphregion(color(white)) xscale(range(1984 2020)) xlabel(1985 1990 1995 2000 2005 2010 2015 2018) ///
		ysc(r(0 90)) ylabel(0(20)90) ///
		xtitle("Year") xscale(titlegap(2)) ytitle("Area (1000 km{superscript:2})")  ///
		legend(col(1) position(3) rowgap(*1.5) ///
		lab(1 "North America") lab(2 "Asia") lab(3 "Europe") ///
		lab(4 "South America") lab(5 "Africa") order(1 2 3 4 5)) ///
		scheme(myplain) xsize(17) ysize(11) /// 
		name(ts_waterClass23_overlayContinent, replace)

cd "$rootDir/Result/resultSTATA" 
graph export ts_waterClass23_overlayContinent.png, name(ts_waterClass23_overlayContinent) replace

** Area graph global
graph twoway (area impute_waterClass_23_thkm2 year, sort fcolor(midblue%80) fintensity(20) lcolor(midblue) lwidth(none)), ///
		graphregion(color(white)) xscale(range(1984 2020)) xlabel(1985 1990 1995 2000 2005 2010 2015 2018) ///
		/*title("Global Surface Water Changes", size(medium)) */ /// 
		xtitle("Year") xscale(titlegap(2)) ytitle("Area (1000 km{superscript:2})")  ///
		scheme(myplain) xsize(17) ysize(12) /// 
		name(ts_waterClass23_global, replace)
		
graph export ts_waterClass23_global.png, name(ts_waterClass23_global) replace


		
cd "$rootDir/$dataDir"
use "dams_training_test_all.dta", clear

** Collapse by year and continent
collapse (sum) impute_waterClass_*, by(year Continent)

** Recode continent for plot order
gen continent_num = .
replace continent_num = 1 if Continent == "North America"
replace continent_num = 2 if Continent == "Asia"
replace continent_num = 3 if Continent == "Europe"
replace continent_num = 4 if Continent == "South America"
replace continent_num = 5 if Continent == "Africa"
replace continent_num = 6 if Continent == "Oceania"
label define continentl 1 "North America" 2 "Asia" 3 "Europe" 4 "South America" 5 "Africa" 6 "Oceania"
label values continent_num continentl

** Convert pixel count to area (km^2)
foreach var of varlist impute_waterClass_* {
	gen `var'_thkm2 = `var' * 900 / 1000000 / 1000
}

graph twoway (area impute_waterClass_23_thkm2 year, sort fcolor(midblue%80) fintensity(20) lcolor(midblue) lwidth(none)), by(continent_num, note("") imargin(b+3)) ///
		graphregion(color(white)) xscale(range(1984 2020)) xlabel(1985(10)2020) ///
		xtitle("Year") xscale(titlegap(2)) ///
		ytitle("") ///
		/* ytitle("Area (1000 km{superscript:2})")  */ ///
		scheme(myplain) xsize(17) ysize(12) /// 
		name(ts_waterClass23_groupContinent, replace)
		 
cd "$rootDir/Result/resultSTATA" 
graph export ts_waterClass23_groupContinent.png, name(ts_waterClass23_groupContinent) replace


*------------------ Plot Results: Map of Water Coverage Change --------------*
cd "$rootDir/$dataDir"
use "dams_training_test_all.dta", clear

** Collapse into Dam-Cumulative Change-Total water coverage 
collapse (firstnm) pre* post* Dam_Name-Main_P_Map, by(Feature_ID)

foreach stat in min max median mean mode{
	foreach i in 1 2 3 23 { 
	rename pre_`stat'_waterClass`i' b`stat'`i' 
	rename post_`stat'_waterClass`i' a`stat'`i' 
		gen d`stat'`i' = a`stat'`i' - b`stat'`i'
	}
}

drop if dmedian23 == . // 373 total obs dropped: 347 dams with no pre-completion observations; 26 dams with no post observations

** Convert pixel count to area (km^2)
foreach var of varlist b* a* d* {
	replace `var' = `var' * 900 / 1000000 
}

** Fix country names for merging
replace Admin0 = "Ecuador" if Admin0 == "Equador"
replace Admin0 = "Laos" if Admin0 == "Lao People's Democratic Republic"
replace Admin0 = "Republic of the Congo" if Admin0 == "Congo"
replace Admin0 = "North Korea" if Admin0 == "Democratic People's Republic of Korea"
replace Admin0 = "Republic of Serbia" if Admin0 == "Serbia"
replace Admin0 = "Syria" if Admin0 == "Syrian Arab Republic"
replace Admin0 = "United States of America" if Admin0 == "United States"
replace Admin0 = "South Korea" if Admin0 == "Republic of Korea"
replace Admin0 = "Ivory Coast" if Admin0 == "C√¥te d'Ivoire"
replace Admin0 =  "Czechia" if Admin0 == "Czech Republic"

** Convert dam attributes to numeric
destring Height, replace force
replace Main_P_Map = "Flood Control" if Main_P_Map == "Flood control"
replace Main_P_Map = "Water Supply" if Main_P_Map == "Water supply"
tab Main_P_Map
gen d_hydro = (Main_P_Map == "Hydroelectricity")
gen d_irrigate = (Main_P_Map == "Irrigation")
gen d_waterSupp = (Main_P_Map == "Water Supply")
gen d_floodContr = (Main_P_Map == "Flood Control")
gen d_other = inlist(Main_P_Map, "Other", "Other/Not Specified")

** Save data for mapping
cd "$rootDir/$dataDir"
save "dams_changeWaterClass_forMap.dta", replace

preserve
keep Feature_ID bmin1-amode23 dmin1-dmode23
export delimited dams_changeWaterClass_forMap.csv, replace
restore

** Collapse by country
preserve
collapse (sum) bmin1-amode23 dmin1-dmode23 Capac Volume_Rep d_hydro d_irrigate d_waterSupp d_floodContr d_other ///
		 (count) Feature_ID ///
		 (mean) Height, ///
		 by(Admin0)
export delimited dams_changeWaterClass_byCountry_forMap.csv, replace
restore

** Collapse by continent 
preserve
collapse (sum) bmin1-amode23 dmin1-dmode23 Capac Volume_Rep d_hydro d_irrigate d_waterSupp d_floodContr d_other ///
		 (count) Feature_ID ///
		 (mean) Height, ///
		 by(Continent)
export delimited dams_changeWaterClass_byContinent_forMap.csv, replace
restore 



*----------------------- Summary Table by Continent ----------------------*
cd "$rootDir/$dataDir"
import delimited dams_changeWaterClass_byContinent_forMap.csv, clear
keep continent feature_id *median23 
gsort -dmedian23
label variable continent "Continent"
label variable bmedian23 "Pre-dam"
label variable amedian23 "Post-dam"
label variable dmedian23 "Difference"
label variable feature_id "Number of Dams"
cd "$rootDir/Result/resultSTATA" 
texsave continent feature_id bmedian23 amedian23 dmedian23 using "sumstat_changeWaterClass_byContinent_v1.tex", replace varlabels


*-------------------- Summary Table by Country (Top 10) ---------------------*
cd "$rootDir/$dataDir"
import delimited dams_changeWaterClass_byCountry_forMap.csv, clear
keep admin0 feature_id *median23 
gsort -dmedian23
label variable admin0 "Country"
label variable bmedian23 "Pre-dam"
label variable amedian23 "Post-dam"
label variable dmedian23 "Difference"
label variable feature_id "Number of Dams"
keep if _n <= 10

cd "$rootDir/Result/resultSTATA" 
texsave admin0 feature_id bmedian23 amedian23 dmedian23 using "sumstat_changeWaterClass_byCountry_v1.tex", replace varlabels




