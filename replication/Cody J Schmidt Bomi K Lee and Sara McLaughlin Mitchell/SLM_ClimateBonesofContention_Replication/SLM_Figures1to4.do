*** SLM, "Climate Bones of Contention"--Figures 

** Install grstyle
*ssc install grstyle, replace
grstyle init
grstyle set plain
grstyle set color Set1
grstyle set symbol
grstyle set lpattern
grstyle set legend 2, nobox

** Figure 1
use "SLM_cru_tmpprec_2001.dta", clear

collapse (mean) tmp s_tmp precip s_pre, by(year)
* Figure 1a
twoway (line precip year) || (line tmp year, yaxis(2) ytitle(" " "Temperature (Degree Celsius)", axis(2)) lpattern(dash)), ///
 title("Aggregated Precipitation and Temperature from 1901 to 2001", size(medium)) ///
 ytitle("Precipitation (Millimeters)" " ") ///
 xtitle("Year") ///
 legend(label(1 "Precipitation") label(2 "Temperature") pos(12) col(2) size(small))
* Figure ab
twoway (line s_pre year) || (line s_tmp year, lpattern(dash)), ///
 title("Aggregated Standardized Precipitation and Temperature" "from 1901 to 2001", size(medium)) ///
 ytitle("Standardized Deviation" " ") ///
 xtitle("Year") ///
 legend(label(1 "Precipitation") label(2 "Temperature") pos(12) col(2) size(small))

 
** Figure 2 & 3: Precipitation in Bolivia and Chile

use "SLM_meansd1ton.dta", clear
tsset ccode year

* Figure 2
twoway tsline s_precn if (ccode==145)&(year<1941)&(year>1909) || (line s_precn year if (ccode==155) &(year<1941)&(year>1909), lpattern(longdash)), ///
xline(1921 1939, lpattern(solid) lcolor(gray)) ///
 title("", size(medium)) ///
 ytitle("Precipitation (std. dev.)" " ") ///
 xtitle("Year") ///
 legend(label(1 "Bolivia") label(2 "Chile") pos(12) col(2) size(small))

* Figure 3
twoway tsline s_precn if (ccode==145)&(year>1979) || (line s_precn year if (ccode==155) &(year>1979), lpattern(longdash)), ///
xline(1999, lpattern(solid) lcolor(gray)) ///
 title("Precipitation in Bolivia and Chile and River Claim Onset, 1981-2001", size(medium)) ///
 ytitle("Precipitation (squared std. dev.)" " ") ///
 xtitle("Year") ///
 legend(label(1 "Bolivia") label(2 "Chile") pos(12) col(2) size(small))
 
** Figure 4a: Precipitation in US and Canada

use "SLM_cru_tmpprec_2001.dta", clear

 twoway tsline precip if (ccode==2), color(black) || (line precip year if (ccode==20), color(gs4) lpattern(dash)), ///
 title("Precipitation", size(medium)) ///
 ytitle("Precipitation (millimeters)" " ") ///
 xtitle("Year") ///
 legend(label(1 "US") label(2 "Canada") pos(12) col(2) size(small)) name(pre2)
 
 twoway tsline s_pre if (ccode==2), color(black) || (line s_pre year if (ccode==20), color(gs4) lpattern(dash)), ///
 title("Standardized Deviation of Precipitation", size(medium)) ///
 ytitle("Precipitation (std. dev.)" " ") ///
 xtitle("Year") ///
 legend(label(1 "US") label(2 "Canada") pos(12) col(2) size(small)) name(predev2)

 graph combine pre2 predev2, t("") 

** Figure 4b: Temperature in US and Canada
 twoway tsline s_tmp if (ccode==2), color(black) || (line s_tmp year if (ccode==20), color(gs4) lpattern(dash)), ///
 title("Standardized Deviation of Temperature", size(medium)) ///
 ytitle("Temperature (std. dev.)" " ") ///
 xtitle("Year") ///
 legend(label(1 "US") label(2 "Canada") pos(12) col(2) size(small)) name(tmpdev)

twoway tsline tmp if (ccode==2), color(black) || (line tmp year if (ccode==20), color(gs4) lpattern(dash)), ///
 title("Temperature", size(medium)) ///
 ytitle("Temperature (Degree Celsius)" " ") ///
 xtitle("Year") ///
 legend(label(1 "US") label(2 "Canada") pos(12) col(2) size(small)) name(tmp)

 graph combine tmp tmpdev, t("") 
