clear all
set more off
capture log close

*---- Set Directory ----*
global rootDir = ""
global rootRA = "" 
global dataRA = "/Data/World/Data/Raw/global_dams"
global dataDir = "Data/Intermediate"
global dataDir1 = "Data/Final"


*---- Log File Path ----*
* cd "`rootDir'/`logDir'"



*******************************************************************************
/*				   		 GLOBAL DAMS TRACKER (GDAT)        					 */
*******************************************************************************

/* 

File Name:    	10_GDAT_Descriptives.do

By: 	    	Alice Tianbo Zhang (tz2218@columbia.edu)      	    

First Edited:  	7/17/18
This Version: 	7/5/21

Purpose:	

Data Used:     

Data Created:   

Program Used:  

*/


*******************************************************************************
/*                       SUMMARY STATISTICS OF DATA	 			   		     */
*******************************************************************************


*----------------------- Save Global Database .dta File ----------------------*

* import excel "$rootDir/$dataDir/GlobalDam_v0.xlsx", sheet("Data_Instrument") firstrow case(lower) clear
* save $rootDir/$dataDir/GlobalDam_v0.dta, replace 


*------------------------- Exploratory Data Analysis -------------------------*
* use $rootDir/$dataDir/GlobalDam_v0_abbVarName.dta, clear
import excel "$rootDir/$dataDir1/v1/GlobalDam_v0_fullVarName_copy.xlsx", firstrow case(lower) clear

* rename *, lower
sort continent admin_country dam_name 
destring completion_year, replace force
replace featureid = _n

tab continent 
gen continent_num = .
replace continent_num = 1 if continent == "North America"
replace continent_num = 2 if continent == "Europe"
replace continent_num = 3 if continent == "Oceania"
replace continent_num = 4 if continent == "Africa"
replace continent_num = 5 if continent == "Asia"
replace continent_num = 6 if continent == "South America"
tab continent_num
label define continent_numl 1 "North America" 2 "Europe" 3 "Oceania" 4 "Africa" 5 "Asia" 6 "South America"
label values continent_num continent_numl

rename main_purpose_mapping Main_P_Map
replace Main_P_Map = "Flood Control" if Main_P_Map == "Flood control"
replace Main_P_Map = "Water Supply" if Main_P_Map == "Water supply"
gen main_purpose_clean = ""
replace main_purpose_clean = "Irrigation" if main_purpose == "Irrigation"
replace main_purpose_clean = "Hydroelectricity" if inlist(main_purpose, "Hydro", "Hydroelecricity", "Hydroelectricity","Hydrolectricity") 
replace main_purpose_clean = "Recreation" if main_purpose == "Recreation"
replace main_purpose_clean = "Water Supply" if inlist(main_purpose, "Water supply", "water supply", "Water Supply", "Water storage")
replace main_purpose_clean = "Flood Control" if inlist(main_purpose, "Flood Control", "Flood control", "Flow Regulation")
replace main_purpose_clean = "Fisheries" if main_purpose == "Fisheries"
replace main_purpose_clean = "Navigation" if main_purpose == "Navigation"
replace main_purpose_clean = "Livestock" if main_purpose == "Livestock"
replace main_purpose_clean = "Other/Not Specified" if inlist(main_purpose, ///
							  "Other", "Other purpose (unspecified)", "Not Specified", "Other/Not Specified", ///
							  "Unspecified", "Unspecified (Other)", "Unspecified purpose")
replace main_purpose_clean = "Other/Not Specified" if inlist(main_purpose, "", " ", ///
							  "Protection of the Environment", "Industrial")
tab main_purpose if main_purpose_clean == ""
replace main_purpose_clean = "Multi-Purpose" if main_purpose_clean == ""
							 
replace reservoir_capacity_reported_offi = reservoir_capacity_reported_offi / 1000 //Convert to km3 
destring dam_height_m, replace force
destring dam_length_m, replace force
destring reservoir_area_reported_official, replace force

cd "$rootDir/$dataDir1/v1/"
save GlobalDam_v0_fullVarName_copy.dta, replace


** Summary table of key attributes
cd "$rootDir/$dataDir1/v1/"
use GlobalDam_v0_fullVarName_copy.dta, clear
tab continent
tab main_purpose_clean

local meanVar completion_year dam_height_m dam_length_m 
local totalVar capacity_mw reservoir_capacity_reported_offi reservoir_area_reported_official
foreach var in `meanVar'{
	sum `var', detail 
}
foreach var in `totalVar'{
	sum `var', detail 
	di `r(sum)'
}

tab main_purpose_clean
gen d_hydro = (main_purpose_clean == "Hydroelectricity")
gen d_irrigate = (main_purpose_clean == "Irrigation")
gen d_waterSupp = (main_purpose_clean == "Water Supply")
gen d_floodContr = (main_purpose_clean == "Flood Control")
gen d_recreate = (main_purpose_clean == "Recreation")
gen d_other = inlist(main_purpose_clean, "Other", "Other/Not Specified")
collapse (count) featureid ///
		 (sum) d_irrigate d_hydro d_waterSupp d_floodContr d_recreate d_other ///
		 (median) completion_year ///
		 (mean) dam_height_m dam_length_m ///
		 (sum) capacity_mw reservoir_capacity_reported_offi reservoir_area_reported_official ///
		 (firstnm) continent_num, by(continent)
 
label variable featureid "Total dam count"
label variable d_irrigat "Irrigation dams"
label variable d_hydro "Hydropower dams"
label variable d_waterSupp "Water supply dams"
label variable d_floodContr "Floor control dams"
label variable d_recreate "Recreation dams"
label variable d_other "Other dams"
label variable completion_year "Median completion year"
label variable dam_height_m "Mean dam height (m)"
label variable dam_length_m "Mean dam length (m)"
label variable reservoir_capacity_reported_offi "Total reservoir capacity (km$^3$)"
label variable reservoir_area_reported_official "Total reservoir area (km$^2$)"
label variable capacity_mw "Total installed capacity (MW)"
label variable continent "Continent"
gsort -featureid


** Pie chart - Dam count by continent  
graph pie, over(continent) plabel(_all percent,format(%3.1f) size(*0.8) color(white)) ///
			graphregion(color(white)) scheme(myplain) ///
			legend(col(1) position(3) rowgap(*1.5)) ///
			title("GDAT by continent")

** Bar graph of dam purpose
* Total
cd "$rootDir/$dataDir1/v1/"
use GlobalDam_v0_fullVarName_copy.dta, clear
graph bar (count), over(main_purpose_clean, sort(1) gap(30)) asyvar ///
				   blabel(bar, size(1.8) format(%9.0fc)) bargap(10) ///
				   ytitle("Number of Observations", height(5)) scheme(myplain) ///
				   yscale (r(0 12000) lcolor(gs10)) ylabel(0(4000)12000) ///
				   ylabel(, nogrid) ///
				   legend(col(1) position(4) rowgap(*1.5) order(8 4 3 10 2 9 5 6 7 1)) ///
			       name(bar_purpose_overall, replace) ///
			       xsize(14) ysize(8)
				   
cd "$rootDir/Result/resultSTATA/"
graph export bar_purpose_total.pdf, replace	
			   
* By continent
graph bar (count), over(main_purpose_clean, sort(1) gap(30)) over(continent_num, gap(*2)) asyvar ///
				   bargap(10) ///
				   ytitle("Number of Observations", height(5)) scheme(myplain) ///
				   yscale (r(0 6000) lcolor(gs10)) ylabel(0(2000)6000) ///
				   ylabel(, nogrid) ///
				   legend(col(1) position(4) rowgap(*1.5)) ///
			       name(bar_purpose_continent, replace) ///
			       xsize(14) ysize(8)

graph export bar_purpose_byContinent.pdf, replace


** Distribution of completion year
hist completion_year if completion_year >= 1900 & completion_year <= 2017 & capacity_mw >= 1, ///
	 by(continent, graphregion(color(white)) note("")) frequency bin(30) fcolor(gs12) lwidth(vthin) ///
	 xscale(range(1900 2020)) xlabel(1900 1930 1960 1990 2017, labsize(small)) ///
	 scheme(myplain) xtitle("Completion Year") ytitle("Number of Observations") name(dist_year, replace)
cd "$rootDir/Result/resultSTATA" 
graph export dist_year_count.pdf, replace
 
hist completion_year if completion_year >= 1900 & completion_year <= 2017 & capacity_mw >= 1, ///
	 by(continent, graphregion(color(white)) note("")) percent bin(30) fcolor(gs12) lwidth(vthin) ///
	 xscale(range(1900 2020)) xlabel(1900 1930 1960 1990 2017, labsize(small)) ///
	 scheme(myplain) xtitle("Completion Year") name(dist_year, replace)

graph export dist_year.pdf, replace

hist completion_year if completion_year >= 1900 & completion_year <= 2017 & capacity_mw >= 1, ///
	 by(continent_num, graphregion(color(white)) note("")) percent bin(30) lwidth(vthin) ///
	 xscale(range(1900 2020)) xlabel(1900 1930 1960 1990 2017, labsize(small)) ///
	 scheme(burd) graphregion(color(none)) plotregion(color(none)) bgcolor(none) ///
	 xtitle("Completion Year") name(dist_year_burd, replace)
	 
graph export dist_year_burd.pdf, replace
graph export dist_year_burd.png, replace
	 
** Cumulative distribution
distplot completion_year if completion_year >= 1900 & completion_year <= 2017 & capacity_mw >= 1, ///
		 over(continent) graphregion(color(white)) xscale(range(1900 2020)) xlabel(1900 1930 1960 1990 2017) ///
		 xtitle("Completion Year") xscale(titlegap(2)) ytitle("Cumulative Share")  ///
		 legend(cols(1) pos(10) ring(0) order(4 3 5 1 2 6)) name(cumDist_year, replace) ///
		 scheme(myplain) 
		 
graph export cumDist_year.pdf, replace

		
distplot completion_year if completion_year >= 1900 & completion_year <= 2017 & capacity_mw >= 1, ///
		 over(continent_num) lwidth(medium medium medium medium medium medium) ///
		 graphregion(color(white)) xscale(range(1900 2020)) xlabel(1900 1930 1960 1990 2017) ///
		 xtitle("Completion Year") xscale(titlegap(2)) ytitle("Cumulative Share")  ///
		 scheme(burd) /// 
		 legend(cols(1) pos(10) ring(0) order(1 2 3 4 5 6)) name(cumDist_year_burd, replace)		 

graph export cumDist_year_burd.pdf, replace
graph export cumDist_year_burd.png, replace
	

		 
*--------------------- Compare GDAT with other databases --------------------*
use "$rootDir/$dataDir1/v1/GlobalDam_v0_fullVarName_copy.dta", clear
		
** GDAT
gen Data = "GDAT"
rename continent Continent
rename featureid Total
rename completion_year Completion_Year
rename decimal_degree_latitude Latitude
rename decimal_degree_longitude Longitude
rename capacity_mw Capacity_MW

* Count of dams by year 
preserve
collapse (count) Total (firstnm) Data, by(Completion_Year)
save "$rootDir/Result/resultSTATA/GDAT_GRanD_AQUASTAT_countbyYear.dta", replace 
restore

* Count of missing values by attribute
preserve
collapse (firstnm) Data ///
		 (count) Total Completion_Year Latitude Longitude Capacity_MW, by(Continent)
save "$rootDir/Result/resultSTATA/GDAT_GRanD_AQUASTAT_countMissing.dta", replace 
restore


** GRanD 
import excel "$rootRA/$dataRA/GRanD_database/GRanD_dams_v1_1_mergeContinent.xlsx", firstrow case(lower) clear
destring year, replace force
replace year = . if year == -99 
destring grand_id, replace force
gen Data = "GRanD"
gen Capacity_MW = .
rename continent Continent
rename grand_id Total
rename year Completion_Year
rename lat_dd Latitude
rename long_dd Longitude

* Count of dams by year
preserve
collapse (count) Total (firstnm) Data, by(Completion_Year)
append using "$rootDir/Result/resultSTATA/GDAT_GRanD_AQUASTAT_countbyYear.dta" 
save "$rootDir/Result/resultSTATA/GDAT_GRanD_AQUASTAT_countbyYear.dta", replace
restore

* Count of missing values by attribute
preserve
collapse (firstnm) Data ///
		 (count) Total Completion_Year Latitude Longitude Capacity_MW, by(Continent)
append using "$rootDir/Result/resultSTATA/GDAT_GRanD_AQUASTAT_countMissing.dta"
save "$rootDir/Result/resultSTATA/GDAT_GRanD_AQUASTAT_countMissing.dta", replace 
restore


** AQUASTAT
import delimited "ne_10m_admin_0_countries.csv", case(lower) clear
tempfile admin0
save `admin0'

import excel "$rootRA/$dataRA/FAO_AQUASTAT/Compiled/global_aquastat_eng.xlsx", firstrow case(lower) clear
rename isoalpha3 adm0_a3
replace adm0_a3 = "SDS" if country == "South Sudan"
merge m:1 adm0_a3 using `admin0'
drop if _merge == 2 
replace continent = "Europe" if country == "French Guiana"
replace continent = "Africa" if country == "Mauritius"
drop _merge

gen Data = "AQUASTAT"
gen Capacity_MW = .
gen Total = _n
rename continent Continent
gen Completion_Year = completedoperationalsince 
rename decimaldegreelatitude Latitude
rename decimaldegreelongitude Longitude
destring Completion_Year, replace force
destring Latitude, replace
destring Longitude, replace

* Count of dams by year
preserve
collapse (count) Total (firstnm) Data, by(Completion_Year)
append using "$rootDir/Result/resultSTATA/GDAT_GRanD_AQUASTAT_countbyYear.dta" 
save "$rootDir/Result/resultSTATA/GDAT_GRanD_AQUASTAT_countbyYear.dta", replace
restore

* Count of missing values by attribute
preserve
collapse (firstnm) Data ///
		 (count) Total Completion_Year Latitude Longitude Capacity_MW, by(Continent)
		 
append using "$rootDir/Result/resultSTATA/GDAT_GRanD_AQUASTAT_countMissing.dta"
save "$rootDir/Result/resultSTATA/GDAT_GRanD_AQUASTAT_countMissing.dta", replace 
restore


** Plot cumulative distribution
use "$rootDir/Result/resultSTATA/GDAT_GRanD_AQUASTAT_countbyYear.dta", clear
capture drop cum_total
sort Data Completion_Year
drop if Completion_Year == . | Completion_Year <= 1800
bysort Data: gen cum_total = sum(Total) 
graph twoway (connected cum_total Completion_Year if Completion_Year >= 1900 & Completion_Year <= 2020 & Data == "GRanD", lpattern(solid) msymbol(o) msize(*0.6)) ///
			 (connected cum_total Completion_Year if Completion_Year >= 1900 & Completion_Year <= 2020 & Data == "AQUASTAT", lpattern(solid) msymbol(o) msize(*0.6)) ///
			 (connected cum_total Completion_Year if Completion_Year >= 1900 & Completion_Year <= 2020 & Data == "GDAT", lpattern(solid) msymbol(o) msize(*0.6)), /// 
			 graphregion(color(white)) xscale(range(1900 2020)) xlabel(1900(10)2020) ///
			 ysc(r(0 25000)) ylabel(0(5000)25000, format(%9.0fc)) ///
			 xtitle("Completion Year") xscale(titlegap(2)) ytitle("Cumulative Count") ///
			 scheme(myplain) xsize(13) ysize(8) ///
			 legend(col(1) position(3) rowgap(*1.5) ///
			 lab(1 "GRanD") lab(2 "AQUASTAT") lab(3 "GDAT") order(3 2 1)) ///
			 name(cumDist_compareData, replace)
			 
cd "$rootDir/Result/resultSTATA/"
graph export cumDist_year_compareData.pdf, replace


** Graph bar plot of missing data - Total
use "$rootDir/Result/resultSTATA/GDAT_GRanD_AQUASTAT_countMissing.dta", clear
replace Capacity_MW = . if Capacity_MW == 0
drop Latitude
rename Longitude Location
foreach var of varlist Total-Capacity_MW{
	rename `var' countm_`var'
}
reshape long countm_, i(Continent Data) j(Attribute) string
encode Data, gen(Data_num)  
recode Data_num (3 = 1) (1 = 2) (2 = 3)
label define datal 1 "GRanD" 2 "AQUASTAT" 3 "GDAT"
label values Data_num datal
replace Attribute = "Completion Year" if Attribute == "Completion_Year"
replace Attribute = "Capacity (MW)" if Attribute == "Capacity_MW"

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

graph bar (sum) countm_, ///
			over(Data_num, gap(10)) over(Attribute) asyvar ///
			blabel(bar, size(vsmall) format(%9.0fc)) bargap(10) ///
			ytitle("Number of Observations", height(5)) scheme(myplain) ///
			yscale (r(0 40000) lcolor(gs10)) ylabel(0(10000)40000) ///
			ylabel(, nogrid) ///
			legend(order(3 2 1) col(1) position(3)) ///
			name(bar_compareTotal, replace) ///
			xsize(13) ysize(8)
			
cd "$rootDir/Result/resultSTATA/"
graph export bar_compareData_total.pdf, replace

** Graph bar plot of missing data - By Continent
graph bar (sum) countm_ if Attribute == "Total", ///
			over(Data_num, gap(30)) over(continent_num, gap(*2)) asyvar ///
			blabel(bar, size(1.8) format(%9.0fc)) bargap(10) ///
			ytitle("Number of Observations", height(5)) scheme(myplain) ///
			yscale (r(0 10000) lcolor(gs10)) ylabel(0(2000)10000) ///
			ylabel(, nogrid) ///
			legend(order(3 2 1) col(1) position(3)) ///
			name(bar_compareContinent, replace) ///
			xsize(13) ysize(8)

graph export bar_compareData_byContinent.pdf, replace


*--------------------- Case Study: Ataturk Dam in Turkey --------------------*
import excel "pixels_sample_v2.xlsx", sheet("Sheet1") firstrow case(lower) clear

* label define periodl 1 "Pre-dam (1984-89)" 2 "Post-dam (1990-94)"
label define periodl 1 "Pre-dam" 2 "Post-dam"
label values period_num periodl

label define waterl 0 "No water" 1 "Water"
label values water_dum waterl

label define water_typel 0 "No water" 1 "Seasonal Water" 2 "Permanent Water"
label values water_type_num water_typel

gen median_pixel_k = median_pixel / 1000

graph bar area, asyvars stack over(water_type_num) over(period_num, gap(*2)) outergap(90) ///
						ytitle("Area (km{superscript:2})") legend(col(1) position(3) order(3 2 1) rowgap(*3)) ///
						ysc(r(0 1000)) ylabel(0(200)1000) xsize(17) ysize(10) ///
						bar(1, bcolor(black%30)) bar(2, bcolor(midblue%30)) bar(3, bcolor(midblue)) ///
						scheme(burd) name(bar_ataturk_areaChange, replace)
		 
graph bar area, asyvars stack over(water_type_num) over(period_num, gap(*2)) outergap(90) ///
						ytitle("Area (km{superscript:2})") legend(row(1) position(6) order(3 2 1) rowgap(*3)) ///
						ysc(r(0 1200)) ylabel(0(200)1200) xsize(17) ysize(15) ///
						bar(1, bcolor(black%30)) bar(2, bcolor(midblue%30)) bar(3, bcolor(midblue)) ///
						scheme(burd) name(bar_ataturk_areaChange, replace)
						

graph export bar_ataturk_areaChange.png, replace


graph bar median_pixel_k, asyvars stack over(water_type_num) over(period_num, gap(*2)) outergap(90) ///
						ytitle("Pixel Count (000s)") yscale(titlegap(4)) legend(col(1) position(3) order(3 2 1) rowgap(*3)) ///
						ysc(r(0 650)) ylabel(0(100)650) xsize(17) ysize(10) ///
						bar(1, bcolor(black%30)) bar(2, bcolor(midblue%30)) bar(3, bcolor(midblue)) ///
						scheme(burd) name(bar_ataturk_pixelChange, replace)
		 

graph export bar_ataturk_pixelChange.png, replace

		 
